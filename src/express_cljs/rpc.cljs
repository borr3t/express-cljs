(ns express-cljs.rpc
  "JSON-RPC 2.0 over both transports: ONE method registry
   served by a single `POST /rpc` route (`http-routes`) and by websocket
   text frames (`serve-ws!`), with the same frames, the same error codes
   (`util/rpc-error-codes`) and the same context-injection contract. Pure
   ClojureScript: no interop — JSON parsing / serialisation goes through
   `express-cljs.ws`, HTTP bodies arrive parsed by the json body parser
   and leave as `{:body frame :json true}`.

   Methods are `(fn [params ctx] -> result | promise)`. `params` is the
   frame's params (a map with keys keywordised as spelled, a vector, or
   nil — never re-cased). `ctx` is the transport context:
   `{:transport :http :request req-map :id .. :method ..}` merged with
   whatever the `:context` fn of `http-routes` returned, or `{:transport
   :ws :conn conn :notify! (fn [method params]) :id .. :method ..}` merged
   with the connection's ctx and the `:context` fn of `serve-ws!`.
   Throw `(error :invalid-params \"..\")` (= `util/rpc-error`) for a
   JSON-RPC error with a chosen code; an argument-error becomes
   -32602, an http-error 401 / 403 becomes -32001 / -32002, anything
   else -32603 with the message hidden unless `:expose-errors?`.

   Frames on the wire (identical over HTTP and ws):
     {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"math/add\",\"params\":{\"a\":1,\"b\":2}}
     {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":3}
     {\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32601,\"message\":\"method not found: x\"}}
     {\"jsonrpc\":\"2.0\",\"method\":\"events/tick\",\"params\":{..}}   (notification, either direction)
   Batches (JSON arrays) are rejected with -32600 and `data.batch? true`.
   A request without `id` is a notification: it is
   dispatched and never answered (HTTP answers 204).

   HTTP statuses: 200 for every answered request — results
   AND method errors — unless `:status-map? true` maps method errors to
   the status column of `util/rpc-error-codes`; transport failures keep
   their HTTP meaning with a JSON-RPC error body: 400 parse error /
   invalid request, 405 non-POST (with Allow), 413 body over :limit, 415
   wrong content type, 204 notification."
  (:refer-clojure :exclude [methods])
  (:require [clojure.string :as str]
            [cljs.core.async :as async]
            [promesa.core :as p]
            [express-cljs.middleware :as mw]
            [express-cljs.route :as route]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]))

(def version
  "The `jsonrpc` member every frame carries."
  "2.0")

(def error
  "`(error code message data?)` — `util/rpc-error`: an ex-info whose :code
   is one of the `util/rpc-error-codes` keys (:invalid-params
   :unauthorized :forbidden :application :timeout ...). Throw it from a
   method (or reject with it) to answer a JSON-RPC error object with that
   code, `message` and `data`."
  util/rpc-error)

;; ---------------------------------------------------------------------------
;; Frames (pure)
;; ---------------------------------------------------------------------------

(defn valid-id?
  "JSON-RPC ids are strings, numbers or null."
  [id]
  (or (nil? id) (string? id) (number? id)))

(defn request-frame
  "`{:jsonrpc \"2.0\" :id id :method method :params params}` (params omitted
   when nil)."
  [id method params]
  (cond-> {:jsonrpc version :id id :method method}
    (some? params) (assoc :params params)))

(defn notification-frame
  "A request frame without :id — the server MUST NOT answer it."
  [method params]
  (cond-> {:jsonrpc version :method method}
    (some? params) (assoc :params params)))

(defn response-frame
  "`{:jsonrpc \"2.0\" :id id :result result}` (result may be nil -> JSON null;
   the member is always present, per spec)."
  [id result]
  {:jsonrpc version :id id :result result})

(defn error-frame
  "`{:jsonrpc \"2.0\" :id id :error {:code n :message s :data d}}`; `code` is
   a `util/rpc-error-codes` keyword or a raw integer; :data only when
   non-nil. `id` is nil for parse / invalid-request errors (spec)."
  ([id code message] (error-frame id code message nil))
  ([id code message data]
   (let [n (if (keyword? code)
             (or (get-in util/rpc-error-codes [code :code])
                 (throw (util/argument-error (str "error-frame: unknown code " (pr-str code)) {:arg :code :value code})))
             code)]
     {:jsonrpc version :id id
      :error (cond-> {:code n :message (str message)}
               (some? data) (assoc :data data))})))

(defn request?
  "True for a frame carrying :method (request or notification)."
  [frame]
  (and (map? frame) (contains? frame :method)))

(defn notification?
  "True for a request frame without an :id member."
  [frame]
  (and (request? frame) (not (contains? frame :id))))

(defn response?
  "True for a frame carrying :result or :error."
  [frame]
  (and (map? frame) (or (contains? frame :result) (contains? frame :error))))

(defn error-response?
  "True for a response frame carrying :error."
  [frame]
  (and (map? frame) (contains? frame :error)))

(defn frame-id
  "The :id a response to `frame` must echo: the frame's id when it is a
   map with a valid id, else nil (the spec's rule for undetectable ids)."
  [frame]
  (when (and (map? frame) (valid-id? (:id frame)))
    (:id frame)))

(defn check-frame
  "Validates a parsed request frame (spec §4): a JSON
   object with `jsonrpc` \"2.0\", a string `method`, a structured `params`
   (object / array / absent) and a valid `id` when present. Returns
   `{:method s :params p :id id :notification? bool}`. Throws an rpc-error
   :invalid-request with :id (nil when undetectable) and :reason; a batch
   array carries `:batch? true`."
  [frame]
  (letfn [(invalid [reason data]
            (throw (util/rpc-error :invalid-request (str "Invalid Request: " reason)
                                   (merge {:id (frame-id frame) :reason reason} data))))]
    (cond
      (sequential? frame) (invalid "batch not supported" {:batch? true :count (count frame)})
      (not (map? frame)) (invalid "frame must be a JSON object" {:frame frame})
      (not= version (:jsonrpc frame)) (invalid "jsonrpc must be \"2.0\"" {})
      (not (and (string? (:method frame)) (not (str/blank? (:method frame))))) (invalid "method must be a non-empty string" {})
      (and (contains? frame :params) (not (or (nil? (:params frame)) (map? (:params frame)) (sequential? (:params frame)))))
      (invalid "params must be an object or an array" {})
      (and (contains? frame :id) (not (valid-id? (:id frame)))) (invalid "id must be a string, a number or null" {})
      :else {:method (:method frame)
             :params (:params frame)
             :id (:id frame)
             :notification? (not (contains? frame :id))})))

(defn error-data
  "The `data` member an express-cljs error contributes to an error frame:
   the :data entry of its ex-data when present, else every other entry
   besides the reserved :type / :code / :message / :id / :cause (so
   `(error :invalid-params \"x\" {:missing [:a]})` and `(error
   :invalid-params \"x\" {:data {:missing [:a]}})` both carry
   `{:missing [:a]}`); nil when there is nothing."
  [e]
  (let [d (or (ex-data e) {})]
    (if (contains? d :data)
      (:data d)
      (not-empty (dissoc d :type :code :message :id :cause :status :expose? :headers)))))

(defn error->frame
  "Any thrown / rejected value from a method -> the JSON-RPC error frame
   answering request `id`: an rpc-error keeps its
   code, message and data (`error-data`); an argument-error is -32602 with its message;
   an http-error 401 / 403 is -32001 / -32002, any other http-error
   -32000 with `{:status n}`; a promesa timeout is -32003; everything
   else -32603 whose message is \"Internal error\" unless
   `:expose-errors?` (then the original message, still no stack)."
  ([id e] (error->frame id e {}))
  ([id e opts]
   (let [d (or (ex-data e) {})
         expose-errors? (or (:expose-errors? opts) (:expose-errors opts))]
     (case (util/error-type e)
       :express-cljs/rpc-error (error-frame id (:code d) (:message d) (error-data e))
       :express-cljs/argument-error (error-frame id :invalid-params (ex-message e) (error-data e))
       :express-cljs/http-error (let [s (:status d)
                                      msg (if (:expose? d) (:message d) (util/reason-phrase s))]
                                  (case s
                                    401 (error-frame id :unauthorized msg (error-data e))
                                    403 (error-frame id :forbidden msg (error-data e))
                                    (error-frame id :application msg (merge {:status s} (error-data e)))))
       (cond
         (instance? p/TimeoutException e) (error-frame id :timeout "method timed out" nil)
         :else (error-frame id :internal-error
                            (if expose-errors?
                              (or (not-empty (str (ex-message e))) (not-empty (str e)) "Internal error")
                              "Internal error")
                            nil))))))

(defn frame->error
  "Inverse of `error->frame` for clients: the :error object of a response
   frame -> rpc-error ex-info with the keyword :code (numeric codes not in
   `util/rpc-error-codes` map to :application with :raw-code)."
  [frame]
  (let [{:keys [code message data]} (:error frame)
        kw (some (fn [[k v]] (when (= (:code v) code) k)) util/rpc-error-codes)]
    (util/rpc-error (or kw :application) (str message)
                    (cond-> {:id (:id frame) :data data}
                      (nil? kw) (assoc :raw-code code)))))

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(def method-option-keys
  "Per-method entry keys: :fn (required), :doc, :timeout-ms."
  #{:fn :doc :timeout-ms})

(def registry-option-keys
  "registry options: :introspection? (adds `rpc.methods`, default false),
   :ping? (adds `rpc.ping`, default true)."
  #{:introspection :ping})

(defn registry?
  "True for a value built by `registry`."
  [v]
  (and (map? v) (true? (:express-cljs/rpc-registry v))))

(defn- check-method-name!
  [n]
  (when-not (and (string? n) (not (str/blank? n)))
    (throw (util/argument-error (str "registry: method names must be non-empty strings, got " (pr-str n)) {:arg :name :value n})))
  (when (str/starts-with? n "rpc.")
    (throw (util/argument-error (str "registry: names starting with \"rpc.\" are reserved by JSON-RPC (" n ")") {:arg :name :value n})))
  n)

(defn- normalize-entry
  [n v]
  (let [entry (cond
                (fn? v) {:fn v}
                (and (map? v) (fn? (:fn v))) (util/check-opts! :method method-option-keys (util/normalize-opts (dissoc v :name)))
                :else (throw (util/argument-error (str "registry: method " (pr-str n) " must be a function or a map with :fn") {:arg :method :value v})))]
    (assoc entry :name n)))

(declare methods)

(defn registry
  "Builds the method registry from `{\"name\" (fn [params ctx]) ...}` or
   `{\"name\" {:fn f :doc \"..\" :timeout-ms 5000} ...}`:
   names are non-empty strings; `rpc.*` is reserved. Built-ins: `rpc.ping`
   (answers {:pong true}) unless `:ping? false`, and `rpc.methods`
   (introspection: a vector of {:name :doc}) with `:introspection? true`.
   Returns `{:express-cljs/rpc-registry true :methods {name entry}}` —
   pass the same value to `http-routes` and `serve-ws!`."
  ([method-map] (registry method-map {}))
  ([method-map opts]
   (when-not (map? method-map)
     (throw (util/argument-error "registry: expected a map of name -> method" {:arg :methods :value method-map})))
   (let [{:keys [introspection ping] :or {ping true}} (util/check-opts! :registry registry-option-keys (util/normalize-opts opts))
         user (into {} (map (fn [[n v]] [(check-method-name! n) (normalize-entry n v)])) method-map)
         reg {:express-cljs/rpc-registry true :methods user}
         reg (if ping
               (assoc-in reg [:methods "rpc.ping"] {:name "rpc.ping" :doc "health check" :fn (fn [_ _] {:pong true})})
               reg)
         reg (if introspection
               (assoc-in reg [:methods "rpc.methods"]
                         {:name "rpc.methods" :doc "lists the registered methods"
                          :fn (fn [_ _] (methods reg))})
               reg)]
     reg)))

(defn methods
  "Sorted vector of `{:name :doc}` for every method in the registry."
  [registry]
  (->> (vals (:methods registry))
       (map (fn [m] {:name (:name m) :doc (:doc m)}))
       (sort-by :name)
       vec))

(defn method
  "The registry entry `{:name :fn :doc :timeout-ms}` for `name`, or nil."
  [registry name]
  (get-in registry [:methods name]))

;; ---------------------------------------------------------------------------
;; Dispatch (shared by both transports)
;; ---------------------------------------------------------------------------

(def call-option-keys
  "call! / http-routes / serve-ws! dispatch options: :timeout-ms (default
   per-call deadline; a method's own :timeout-ms wins), :expose-errors?
   (show non-RPC error messages in -32603 frames, default false), :on-error
   (fn [err ctx] called for every error frame produced and for failed
   notifications, which get no frame), :batch? (reserved marker: only
   false is accepted)."
  #{:timeout-ms :expose-errors :on-error :batch})

(defn- check-call-opts!
  [op opts]
  (let [o (util/check-opts! op call-option-keys (util/normalize-opts opts))]
    (when (true? (:batch o))
      (throw (util/argument-error (str (name op) ": JSON-RPC batch requests are not supported; :batch? is a reserved marker")
                                  {:arg :batch})))
    o))

(defn- report!
  [{:keys [on-error]} e ctx]
  (when on-error (try (on-error e ctx) (catch :default _ nil))))

(defn call!
  "Dispatches ONE parsed request frame against the registry and resolves
   with the response frame (result or error), or nil for a notification
   (never answered, spec §4.1; its errors go to :on-error only). `ctx` is
   merged with `{:id :method :notification?}` before the method sees it.
   Invalid frames (-32600, batches included) always get an answer with
   the detectable id or null. A method's :timeout-ms (or opts
   :timeout-ms) turns a slow call into -32003. Never rejects."
  ([registry frame ctx] (call! registry frame ctx {}))
  ([registry frame ctx opts]
   (let [opts (check-call-opts! :call opts)
         id (frame-id frame)
         notification? (notification? frame)]
     (-> (p/do
           (let [{:keys [method params id] :as req} (check-frame frame)
                 entry (or (get-in registry [:methods method])
                           (throw (util/rpc-error :method-not-found (str "method not found: " method) {:id id :method method})))
                 ctx (merge ctx {:id id :method method :notification? (:notification? req) :params params})
                 timeout (or (:timeout-ms entry) (:timeout-ms opts))
                 result (p/do ((:fn entry) params ctx))
                 result (if (and (number? timeout) (pos? timeout)) (p/timeout result timeout) result)]
             (p/then result (fn [r] (when-not (:notification? req) (response-frame id r))))))
         (p/catch (fn [e]
                    (let [invalid? (= :invalid-request (util/rpc-error-code e))
                          frame (error->frame id e opts)]
                      (report! opts e (assoc ctx :frame frame))
                      (if (and notification? (not invalid?))
                        nil
                        frame))))))))

;; ---------------------------------------------------------------------------
;; HTTP transport
;; ---------------------------------------------------------------------------

(def http-option-keys
  "http-routes options: :path (default \"/rpc\"), :context (fn [req-map] ->
   map | promise, merged into ctx; a thrown http-error answers at HTTP
   level), :limit (json body limit, default \"256kb\"),
   :status-map? (true: method-error frames take the HTTP status of
   `util/rpc-error-codes`; a map {code-keyword status} overrides entries),
   plus the dispatch options of `call-option-keys`."
  (into call-option-keys #{:path :context :limit :status-map}))

(defn error-status
  "The HTTP status for an error frame under a :status-map option (false ->
   200; true -> the `util/rpc-error-codes` status of the frame's code; a
   map -> its entry for the code keyword, else the table)."
  [frame status-map]
  (let [code (get-in frame [:error :code])
        kw (some (fn [[k v]] (when (= (:code v) code) k)) util/rpc-error-codes)]
    (cond
      (not status-map) 200
      (map? status-map) (or (get status-map kw) (get-in util/rpc-error-codes [kw :status]) 500)
      :else (get-in util/rpc-error-codes [kw :status] 500))))

(defn- transport-response
  "A JSON-RPC error frame as an HTTP transport failure (400 / 413 / 415)."
  [status code message data]
  {:status status :body (error-frame nil code message data) :json true})

(defn- body-error->response
  "body-parser / wrapper errors under the RPC path -> transport failure
   frames (400 parse error, 413 too large, 415 charset); anything else
   passes on (nil)."
  [err]
  (let [d (or (ex-data err) {})]
    (when (= :express-cljs/http-error (:type d))
      (case (:reason d)
        :parse-failed (transport-response 400 :parse-error "Parse error" {:reason "malformed JSON body"})
        :too-large (transport-response 413 :invalid-request "Invalid Request: body too large" {:reason "body too large" :limit (:limit d)})
        :charset-unsupported (transport-response 415 :invalid-request "Invalid Request: unsupported charset" {:reason "unsupported charset"})
        :encoding-unsupported (transport-response 415 :invalid-request "Invalid Request: unsupported content encoding" {:reason "unsupported content encoding"})
        nil))))

(defn http-handler
  "The `(fn [req-map] -> response-map)` behind `http-routes`, exposed for
   tests and custom mounting: expects :body parsed by the json parser
   (see `http-routes` for the parser options)."
  [registry {:keys [context status-map] :as opts}]
  (let [call-opts (select-keys opts call-option-keys)]
    (fn [req]
      (let [ct (str (:content-type req))
            body (:body req)]
        (cond
          (not (str/includes? ct "json"))
          (transport-response 415 :invalid-request "Invalid Request: expected application/json" {:reason "content type" :content-type (:content-type req)})

          (nil? body)
          (transport-response 400 :invalid-request "Invalid Request: empty body" {:reason "empty body"})

          :else
          (p/let [extra (if context (context req) {})
                  ctx (merge {:transport :http :request req :notify! nil} (when (map? extra) extra))
                  frame (call! registry body ctx call-opts)]
            (cond
              (nil? frame) {:status 204 :end true}
              (= -32600 (get-in frame [:error :code])) {:status 400 :body frame :json true}
              (error-response? frame) {:status (error-status frame status-map) :body frame :json true}
              :else {:status 200 :body frame :json true})))))))

(defn http-routes
  "Route entries mounting the registry at `:path` (default \"/rpc\") as ONE
   endpoint: a `route/resource` answering POST (other
   methods get 405 with Allow) through a json body parser with `:limit`
   (default \"256kb\") in non-strict mode (scalars reach frame validation
   as -32600, not as a parse error) and the `http-handler`, plus an
   error entry translating body-parser failures into JSON-RPC transport
   frames (400 parse error, 413, 415). Splice the result into the app's
   route sources: `(core/app opts routes (rpc/http-routes methods {..}))`.
   Options: `http-option-keys`."
  ([registry] (http-routes registry {}))
  ([registry opts]
   (when-not (registry? registry)
     (throw (util/argument-error "http-routes: expected a registry from rpc/registry" {:arg :registry})))
   (let [opts (util/check-opts! :http-routes http-option-keys (util/normalize-opts opts))
         _ (check-call-opts! :http-routes (select-keys opts call-option-keys))
         {:keys [path limit] :or {path "/rpc" limit "256kb"}} opts
         parser (mw/json {:limit limit :strict? false :type ["application/json" "application/*+json"]})
         handler (http-handler registry opts)]
     (route/routes
      (route/resource path {:post [parser handler]})
      (route/error path (fn [err _req] (body-error->response err)))))))

;; ---------------------------------------------------------------------------
;; Websocket transport
;; ---------------------------------------------------------------------------

(defn notify!
  "Server -> client notification: sends `{:jsonrpc \"2.0\" :method method
   :params params}` on the connection (the streaming primitive). Resolves
   like `ws/send!`."
  [conn method params]
  (ws/send-json! conn (notification-frame method params)))

(defn broadcast-notify!
  "Sends a notification frame to every connection of a ws-server (`:all`),
   a topic or a collection of connections (see `ws/broadcast!`)."
  ([ws-server method params] (broadcast-notify! ws-server :all method params))
  ([ws-server target method params]
   (ws/broadcast! ws-server target {:json (notification-frame method params)})))

(def ws-option-keys
  "serve-ws! options: :context (fn [conn] -> map | promise, merged into
   every call's ctx after the connection's :ctx), :strict? (close with
   3001 on a frame that is not JSON, default false: answer -32700 and keep
   the socket), :on-open (fn [conn]) / :on-close (fn [conn close-info])
   hooks, plus `call-option-keys`."
  (into call-option-keys #{:context :strict :on-open :on-close}))

(defn- handle-ws-frame!
  "One inbound message -> at most one outbound frame; concurrent calls on
   a connection never wait for each other."
  [registry conn msg {:keys [context strict] :as opts} call-opts]
  (let [base-ctx {:transport :ws :conn conn :notify! (fn [method params] (notify! conn method params))}
        respond (fn [frame]
                  (when (some? frame)
                    (-> (ws/send-json! conn frame)
                        (p/catch (fn [_] nil)))))]
    (if (= :binary (:type msg))
      (respond (error-frame nil :invalid-request "Invalid Request: binary frames are not JSON-RPC" {:reason "binary frame"}))
      (let [parsed (try (ws/parse-json (:data msg)) (catch :default e e))]
        (if (some? (ex-data parsed))
          (do (report! call-opts parsed {:transport :ws :conn conn})
              (if strict
                (ws/close! conn 3001 "protocol violation: non-JSON frame")
                (respond (error-frame nil :parse-error "Parse error" {:reason (ex-message parsed)}))))
          (-> (p/let [extra (if context (context conn) {})
                      ctx (merge (ws/ctx conn) (when (map? extra) extra) base-ctx)
                      frame (call! registry parsed ctx call-opts)]
                (respond frame))
              (p/catch (fn [e]
                         (report! call-opts e {:transport :ws :conn conn})
                         (respond (error->frame (frame-id parsed) e call-opts))))))))))

(defn serve-ws!
  "Serves the registry on every connection of a `ws/serve!` ws-server:
   each inbound text frame is parsed and dispatched with `call!`
   (concurrently — a slow method never blocks the next frame), request
   frames are answered on the same connection with the echoed id,
   notifications are dispatched and not answered, malformed JSON gets a
   -32700 frame with id null, binary frames -32600. Every method sees
   `{:transport :ws :conn conn :notify! f :id ..}` merged over the
   connection's :ctx (the `:authenticate` result) and the `:context` fn
   result. Uses the lossless `ws/on-open!` hook and the per-connection
   IN channel (so it owns the messages of every connection: raw-frame
   consumers and the RPC layer do not share one endpoint). Returns
   `{:ws-server .. :registry .. :stop! (fn [])}`; `core/close!` needs
   nothing more."
  ([ws-server registry] (serve-ws! ws-server registry {}))
  ([ws-server registry opts]
   (when-not (registry? registry)
     (throw (util/argument-error "serve-ws!: expected a registry from rpc/registry" {:arg :registry})))
   (let [opts (util/check-opts! :serve-ws ws-option-keys (util/normalize-opts opts))
         call-opts (check-call-opts! :serve-ws (select-keys opts call-option-keys))
         {:keys [on-open on-close]} opts
         attach! (fn [conn]
                   (when on-open (on-open conn))
                   (let [in (ws/messages conn)]
                     (letfn [(pump []
                               (async/take! in (fn [msg]
                                                 (when (some? msg)
                                                   (handle-ws-frame! registry conn msg opts call-opts)
                                                   (pump)))))]
                       (pump))))
         off-open (ws/on-open! ws-server attach!)
         off-close (ws/on-close! ws-server (fn [conn info] (when on-close (on-close conn info))))]
     (doseq [c (ws/conns ws-server)] (attach! c))
     {:ws-server ws-server
      :registry registry
      :stop! (fn [] (off-open) (off-close) nil)})))

;; ---------------------------------------------------------------------------
;; Client helper — over a ws connection (server or client
;; side): request/response correlation, notifications both ways.
;; ---------------------------------------------------------------------------

(def client-option-keys
  "client options: :timeout-ms (per call, default 10000), :on-notification
   (fn [frame]) for server -> client notifications (they also arrive on
   the client's :notifications channel), :id-prefix (string ids instead
   of counters)."
  #{:timeout-ms :on-notification :id-prefix})

(defn client
  "A JSON-RPC client over a connection map (typically from `ws/connect!`):
   returns `{:call! (fn [method params?] -> promise of result) :notify!
   (fn [method params?] -> promise) :notifications <chan of inbound
   notification frames> :pending (fn [] -> n) :close! (fn [] -> promise)}`.
   Ids are counters (or `:id-prefix` + counter); a response rejects with
   `frame->error` for error frames, a call rejects with an rpc-error
   :timeout after :timeout-ms (default 10000; timers are cancelled when the
   answer arrives), and every pending call rejects when the connection
   closes. Takes over the connection's IN channel."
  ([conn] (client conn {}))
  ([conn opts]
   (let [{:keys [timeout-ms on-notification id-prefix] :or {timeout-ms 10000}}
         (util/check-opts! :client client-option-keys (util/normalize-opts opts))
         state (atom {:seq 0 :pending {}})
         notifications (async/chan (async/sliding-buffer 256))
         in (ws/messages conn)
         settle! (fn [frame]
                   (let [id (:id frame)]
                     (when-let [d (get-in @state [:pending id])]
                       (swap! state update :pending dissoc id)
                       (if (error-response? frame)
                         (p/reject d (frame->error frame))
                         (p/resolve d (:result frame))))))
         reject-all! (fn [e]
                       (let [pending (:pending @state)]
                         (swap! state assoc :pending {})
                         (doseq [[_ d] pending] (p/reject d e))
                         (async/close! notifications)))]
     (letfn [(pump []
               (async/take! in (fn [msg]
                                 (if (nil? msg)
                                   (reject-all! (util/ws-error "connection closed" {:code :closed :conn-id (:id conn)}))
                                   (do (when (= :text (:type msg))
                                         (let [frame (try (ws/parse-json (:data msg)) (catch :default _ nil))]
                                           (cond
                                             (response? frame) (settle! frame)
                                             (notification? frame) (do (async/put! notifications frame)
                                                                       (when on-notification (on-notification frame)))
                                             :else nil)))
                                       (pump))))))]
       (pump))
     {:call! (fn call!*
              ([method] (call!* method nil))
              ([method params]
               (let [n (:seq (swap! state update :seq inc))
                     id (if id-prefix (str id-prefix n) n)
                     d (p/deferred)]
                 (swap! state assoc-in [:pending id] d)
                 (-> (ws/send-json! conn (request-frame id method params))
                     (p/then (fn [_] (p/timeout d timeout-ms)))
                     (p/catch (fn [e]
                                (swap! state update :pending dissoc id)
                                (throw (if (instance? p/TimeoutException e)
                                         (util/rpc-error :timeout (str "no response to " method " within " timeout-ms " ms") {:id id :method method})
                                         e))))))))
      :notify! (fn notify!*
                 ([method] (notify!* method nil))
                 ([method params] (ws/send-json! conn (notification-frame method params))))
      :notifications notifications
      :pending (fn [] (count (:pending @state)))
      :close! (fn [] (ws/close! conn 1000 "client done"))})))
