(ns express-cljs.ws
  "WebSockets on the wrapper's HTTP server through the `ws` package
   (inventory §2.3 / §6 / §12.4). This is the SECOND and last binding
   namespace: every line of `ws` and raw-socket interop lives here (house
   style `js-invoke` + `goog.object`, no `^js`, no dot-forms); `rpc` and
   application code only see the values below.

   Values:

   - a WS-SERVER value from `serve!`: `{:wss <opaque WebSocketServer>
     :server <core server handle> :opts .. :path .. :state <atom>
     :connections <core.async chan>}`. Pass it to `connections`, `conns`,
     `conn-count`, `broadcast!`, `join!` / `leave!` / `emit!`, `on-open!` /
     `on-close!`, `stop!`.
   - a CONNECTION map, one per accepted socket (server side, from the
     `connections` channel / `on-open!`) or per `connect!` (client side):
     `{:id :ctx :remote-addr :remote-port :headers :protocol (:ws | :wss)
     :subprotocol :upgrade <request map> :in <chan> :out <chan>
     :opened-at}`. Its metadata `:express-cljs/raw` carries the socket, so
     `send!`, `close!`, `ping!`, `messages`, `closed` need only the map.

   Channels (inventory 11.5): `(messages conn)` yields `{:type :text :data
   \"..\"}` / `{:type :binary :data <Buffer>}` (plus :json with the
   `:decode :json` option) and closes when the socket closes; a full
   buffer pauses the socket (receive backpressure). `(out conn)` sends
   whatever is put on it (see `send!` for the payload forms).
   `(connections ws-server)` yields `{:event :open :conn c}`, `{:event
   :close :conn c :code n :reason s}` and `{:event :error :conn c :error e}`
   (sliding buffer: consume promptly or use the lossless `on-open!` /
   `on-close!` callbacks). Sends are promises, never channels.

   Lifecycle: `serve!` binds ONE Node 'upgrade' listener per http.Server
   that dispatches by pathname across every `serve!` on that server and
   answers 404 for the rest; `:authenticate` runs before the handshake and
   can answer 401 / 403 as real HTTP responses; `serve!` also registers a
   `core/add-close-hook!` so `core/close!` closes every client with 1001,
   waits :ws-close-timeout-ms, terminates the leftovers and detaches
   (inventory 5.3). The keepalive interval is unref'd (6.7)."
  (:require [clojure.string :as str]
            [cljs.core.async :as async]
            [goog.object :as gobj]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Module and interop helpers
;; ---------------------------------------------------------------------------

(def ^:private ws-module (js/require "ws"))

(defn- jsget
  "Property read without interop syntax at the call site; nil-safe."
  [o & path]
  (when (some? o) (apply gobj/getValueByKeys o path)))

(defn- jsset!
  [o k v]
  (gobj/set o k v)
  o)

(def ^:private upgrade-key "__expressCljsUpgrade")

(def ready-states
  "ws readyState number -> keyword."
  {0 :connecting 1 :open 2 :closing 3 :closed})

;; ---------------------------------------------------------------------------
;; JSON (the only JSON interop the RPC layer needs; keys as spelled, 11.1)
;; ---------------------------------------------------------------------------

(defn- plain-object?
  [v]
  (and (some? v)
       (= "object" (goog/typeOf v))
       (let [proto (js-invoke js/Object "getPrototypeOf" v)]
         (or (nil? proto) (identical? proto (jsget js/Object "prototype"))))))

(defn- ->clj*
  [v]
  (cond
    (array? v) (mapv ->clj* (array-seq v))
    (plain-object? v) (into {} (map (fn [k] [(keyword k) (->clj* (gobj/get v k))])) (js-keys v))
    :else v))

(defn ->json
  "Serialises a CLJS value as a JSON string (keywords render as their
   name, keys are never re-cased, inventory 11.1)."
  [v]
  (js-invoke js/JSON "stringify" (clj->js v :keyword-fn name)))

(defn parse-json
  "Parses a JSON string into CLJS data with keys keywordised AS SPELLED
   (`{\"maxAge\":1}` -> {:maxAge 1}); arrays become vectors. Throws a
   ws-error {:code :invalid-json} on malformed input (the RPC layer maps
   it to -32700). A message map (`{:type :text :data ..}`) may be passed
   instead of the string."
  [s]
  (let [s (if (map? s) (:data s) s)]
    (try
      (->clj* (js-invoke js/JSON "parse" s))
      (catch :default e
        (throw (util/ws-error (str "invalid JSON: " (jsget e "message"))
                              {:code :invalid-json :cause e}))))))

(def json
  "Alias of `parse-json` (inventory 12.4 name)."
  parse-json)

;; ---------------------------------------------------------------------------
;; Errors and close codes (inventory 6.6 / 6.12 / 8.4)
;; ---------------------------------------------------------------------------

(defn valid-close-code?
  "RFC 6455 codes ws accepts in `close(code)`: 1000..1014 except 1004 /
   1005 / 1006, or 3000..4999 (pitfall 9.22)."
  [code]
  (and (integer? code)
       (or (and (<= 1000 code 1014) (not (contains? #{1004 1005 1006} code)))
           (<= 3000 code 4999))))

(def ws-error-close-codes
  "The close code ws sends to the peer after a receiver error, by the
   kebab `WS_ERR_*` code (ws/lib/receiver.js: 1009 for length limits,
   1007 for UTF-8, 1002 for every framing violation). The SERVER-side
   'close' event still reports 1006 (the receiver stops parsing before
   the peer's reply; uncertainty 10.2) — the client sees this code."
  {:unsupported-message-length 1009
   :unsupported-data-payload-length 1009
   :too-many-buffered-parts 1009
   :invalid-utf8 1007})

(defn- err-code-keyword
  "\"WS_ERR_UNSUPPORTED_MESSAGE_LENGTH\" -> :unsupported-message-length;
   other JS error codes (\"ECONNRESET\") kebab-cased; nil when absent."
  [code]
  (when (and (string? code) (not (str/blank? code)))
    (keyword (str/lower-case (str/replace (str/replace code #"^WS_ERR_" "") "_" "-")))))

(defn ->ws-error
  "A JS error from a socket 'error' event -> ws-error ex-info {:code
   <kebab WS_ERR code, kebab socket error code, or :socket> :close-code
   <what the peer receives: only for WS_ERR_* receiver errors, 1002 unless
   listed in ws-error-close-codes> :conn-id .. :cause err}. express-cljs
   errors pass through."
  [err conn]
  (if (some? (ex-data err))
    err
    (let [code (jsget err "code")
          kw (err-code-keyword code)
          receiver? (and (string? code) (str/starts-with? code "WS_ERR_"))]
      (util/ws-error (str (jsget err "message"))
                     {:code (or kw :socket)
                      :close-code (when receiver? (get ws-error-close-codes kw 1002))
                      :conn-id (:id conn)
                      :cause err}))))

;; ---------------------------------------------------------------------------
;; Connection maps
;; ---------------------------------------------------------------------------

(defn- raw-of
  "The {:ws socket :state atom :wsv ws-server-or-nil :opts} behind a
   connection map (argument-error otherwise)."
  [conn]
  (or (:express-cljs/raw (meta conn))
      (throw (util/argument-error "not a connection map produced by express-cljs.ws" {:arg :conn :value conn}))))

(defn- sock [conn] (:ws (raw-of conn)))
(defn- conn-state [conn] (:state (raw-of conn)))

(defn ready-state
  "One of :connecting :open :closing :closed."
  [conn]
  (get ready-states (jsget (sock conn) "readyState") :closed))

(defn open?
  "True while frames can be sent."
  [conn]
  (= :open (ready-state conn)))

(defn buffered-amount
  "Bytes queued on the socket and not yet flushed (inventory 6.8)."
  [conn]
  (or (jsget (sock conn) "bufferedAmount") 0))

(defn messages
  "The connection's IN channel: `{:type :text :data \"..\"}` or `{:type
   :binary :data <Buffer>}` (with the server / client option `:decode
   :json`, text frames also carry :json — the parsed value — or
   :json-error). Closed when the socket closes; closing it yourself
   unsubscribes (later frames are discarded)."
  [conn]
  (:in conn))

(defn out
  "The connection's OUT channel: every value put on it goes through
   `send!` in order (failures are reported as {:event :error} on the
   server's connections channel). Closed when the socket closes."
  [conn]
  (:out conn))

(defn ctx
  "The context the `:authenticate` function returned for this connection
   ({} when there is no authenticate step)."
  [conn]
  (:ctx conn))

(defn upgrade
  "The upgrade request map (headers, cookies, query, ip, ... — see
   `core/upgrade-request->map`) of a server-side connection; nil for a
   client connection."
  [conn]
  (:upgrade conn))

(defn closed
  "Promise resolved with `{:code n :reason s :forced? bool}` when the
   socket's 'close' event has fired (already resolved for a closed
   connection). Never rejects."
  [conn]
  (:closed @(conn-state conn)))

(defn- payload
  "Payload forms of `send!` -> [data binary?]."
  [data]
  (cond
    (string? data) [data false]
    (core/buffer? data) [data true]
    (instance? js/ArrayBuffer data) [data true]
    (and (map? data) (contains? data :text)) [(str (:text data)) false]
    (and (map? data) (contains? data :binary)) [(:binary data) true]
    (and (map? data) (contains? data :json)) [(->json (:json data)) false]
    (keyword? data) [(name data) false]
    :else [(->json data) false]))

(defn- byte-length
  [d]
  (if (string? d) (js-invoke js/Buffer "byteLength" d "utf8") (or (jsget d "byteLength") (jsget d "length") 0)))

(defn send!
  "Sends one frame; resolves `{:bytes n}` when the socket has written it.
   Payloads: a string -> text frame; a Buffer / Uint8Array / ArrayBuffer ->
   binary frame; `{:text s}` / `{:binary bytes}` / `{:json v}` explicit
   envelopes; any other CLJS value -> JSON text (`send-json!` is the
   explicit form for maps). Rejects with a ws-error {:code :closed} when
   the connection is not open and {:code :backpressure} when
   `bufferedAmount` exceeds :max-buffered-bytes (default 8 MiB) instead of
   buffering without bound (inventory 6.8)."
  [conn data]
  (let [{:keys [ws opts]} (raw-of conn)
        [d binary?] (payload data)
        max-buf (get opts :max-buffered-bytes 8388608)
        buffered (or (jsget ws "bufferedAmount") 0)]
    (cond
      (not= 1 (jsget ws "readyState"))
      (p/rejected (util/ws-error "connection is not open"
                                 {:code :closed :ready-state (ready-state conn) :conn-id (:id conn)}))

      (> buffered max-buf)
      (p/rejected (util/ws-error (str "send buffer over limit (" buffered " > " max-buf " bytes)")
                                 {:code :backpressure :buffered-amount buffered :max-buffered-bytes max-buf :conn-id (:id conn)}))

      :else
      (p/create
       (fn [resolve reject]
         (js-invoke ws "send" d #js {:binary binary?}
                    (fn [err]
                      (if (some? err)
                        (reject (->ws-error err conn))
                        (resolve {:bytes (byte-length d)})))))))))

(defn send-json!
  "Sends any CLJS value as a JSON text frame (see `send!`)."
  [conn v]
  (send! conn {:json v}))

(defn send-binary!
  "Sends a Buffer / Uint8Array / ArrayBuffer as a binary frame (see `send!`)."
  [conn bytes]
  (send! conn {:binary bytes}))

(defn- check-close-args!
  [code reason]
  (when (and (some? code) (not (valid-close-code? code)))
    (throw (util/ws-error (str "invalid close code " (pr-str code) " (1000..1014 minus 1004/1005/1006, or 3000..4999)")
                          {:code :invalid-close-code :close-code code})))
  (when (and (some? reason) (> (js-invoke js/Buffer "byteLength" (str reason) "utf8") 123))
    (throw (util/ws-error "close reason must be at most 123 UTF-8 bytes"
                          {:code :invalid-close-reason :reason reason}))))

(defn close!
  "Starts the closing handshake with `code` (default 1000) and `reason`
   (<= 123 bytes) and resolves with the 'close' event `{:code :reason
   :forced?}`. If the peer has not answered after :timeout-ms (default
   the connection's :close-timeout-ms, 2000) the socket is terminated —
   the wrapper's deadline wins over ws's own 30 s closeTimeout
   (uncertainty 10.17; ws guards against a second 'close' event). Throws
   a ws-error {:code :invalid-close-code | :invalid-close-reason}
   synchronously for a code ws would reject (pitfall 9.22). Idempotent:
   an already closed connection resolves at once."
  ([conn] (close! conn 1000 nil {}))
  ([conn code] (close! conn code nil {}))
  ([conn code reason] (close! conn code reason {}))
  ([conn code reason {:keys [timeout-ms]}]
   (check-close-args! code reason)
   (let [{:keys [ws state opts]} (raw-of conn)
         rs (jsget ws "readyState")
         timeout-ms (or timeout-ms (get opts :close-timeout-ms 2000))]
     (cond
       (= 3 rs) (:closed @state)
       :else
       (do (when (< rs 2)
             (if (some? reason)
               (js-invoke ws "close" (or code 1000) (str reason))
               (js-invoke ws "close" (or code 1000))))
           (p/create
            (fn [resolve _]
              (let [timer (js/setTimeout (fn []
                                           (when (not= 3 (jsget ws "readyState"))
                                             (swap! state assoc :forced? true)
                                             (js-invoke ws "terminate")))
                                         timeout-ms)]
                (js-invoke timer "unref")
                (p/then (:closed @state)
                        (fn [r] (js/clearTimeout timer) (resolve r)))))))))))

(defn terminate!
  "Destroys the socket immediately (both sides see 1006). Resolves with the
   'close' event like `close!`."
  [conn]
  (let [{:keys [ws state]} (raw-of conn)]
    (when (not= 3 (jsget ws "readyState"))
      (swap! state assoc :forced? true)
      (js-invoke ws "terminate"))
    (:closed @state)))

(defn ping!
  "Sends a ping frame and resolves with the round-trip time in
   milliseconds when the matching pong arrives (ws answers pings
   automatically on both sides, inventory 6.7). `data` (default a
   counter) is the ping payload echoed in the pong. Rejects with a
   ws-error {:code :closed} when not open and {:code :timeout} after
   :timeout-ms (default 5000, unref'd timer)."
  ([conn] (ping! conn nil {}))
  ([conn data] (ping! conn data {}))
  ([conn data {:keys [timeout-ms] :or {timeout-ms 5000}}]
   (let [{:keys [ws state]} (raw-of conn)]
     (if (not= 1 (jsget ws "readyState"))
       (p/rejected (util/ws-error "connection is not open" {:code :closed :conn-id (:id conn)}))
       (let [n (:ping-seq (swap! state update :ping-seq (fnil inc 0)))
             data (if (some? data) (str data) (str "express-cljs-" n))
             start (js/Date.now)
             d (p/deferred)]
         (swap! state assoc-in [:pings data] d)
         (let [timer (js/setTimeout (fn []
                                      (swap! state update :pings dissoc data)
                                      (p/reject d (util/ws-error "pong not received in time"
                                                                  {:code :timeout :timeout-ms timeout-ms :conn-id (:id conn)})))
                                    timeout-ms)]
           (js-invoke timer "unref")
           (js-invoke ws "ping" data)
           (p/then d (fn [_] (js/clearTimeout timer) (- (js/Date.now) start)))))))))

;; ---------------------------------------------------------------------------
;; Socket -> connection wiring (shared by server and client side)
;; ---------------------------------------------------------------------------

(defn- message->map
  [data binary? decode]
  (if binary?
    {:type :binary :data data}
    (let [s (js-invoke data "toString" "utf8")
          m {:type :text :data s}]
      (if (= :json decode)
        (try (assoc m :json (parse-json s))
             (catch :default e (assoc m :json-error (ex-message e))))
        m))))

(defn- pump-out!
  "Sends whatever is put on the OUT channel, in order, until it closes."
  [conn on-error]
  (let [ch (:out conn)]
    (letfn [(pump []
              (async/take! ch (fn [v]
                                (when (some? v)
                                  (-> (send! conn v)
                                      (p/catch (fn [e] (on-error e)))
                                      (p/finally (fn [_ _] (pump))))))))]
      (pump))))

(defn- wrap-socket!
  "Turns a ws socket into a connection map with its listeners: message ->
   IN channel (offer, else pause until the consumer takes), pong -> alive +
   pending pings, error -> `emit`, close -> channels closed, deferred
   resolved, `on-close` called. `fields` are the public map entries."
  [ws fields opts {:keys [emit on-close]}]
  (let [state (atom {:alive true :pings {} :closed (p/deferred) :forced? false})
        in (async/chan (get opts :message-buffer 64))
        out-ch (async/chan 64)
        conn (with-meta (assoc fields :in in :out out-ch :opened-at (js/Date.now))
               {:express-cljs/raw {:ws ws :state state :opts opts}})
        emit (or emit (fn [_]))]
    (js-invoke ws "on" "message"
               (fn [data binary?]
                 (let [m (message->map data binary? (:decode opts))]
                   (when-not (async/offer! in m)
                     ;; buffer full (or channel closed): pause until the put lands
                     (js-invoke ws "pause")
                     (async/put! in m (fn [_] (when (not= 3 (jsget ws "readyState")) (js-invoke ws "resume"))))))))
    (js-invoke ws "on" "pong"
               (fn [data]
                 (swap! state assoc :alive true)
                 (let [k (js-invoke data "toString" "utf8")]
                   (when-let [d (get-in @state [:pings k])]
                     (swap! state update :pings dissoc k)
                     (p/resolve d k)))))
    (js-invoke ws "on" "error"
               (fn [err]
                 (let [e (->ws-error err conn)]
                   (swap! state assoc :error e)
                   (emit {:event :error :conn conn :error e}))))
    (js-invoke ws "on" "close"
               (fn [code reason]
                 (let [r {:code code
                          :reason (if (some? reason) (js-invoke reason "toString" "utf8") "")
                          :forced? (boolean (:forced? @state))}
                       r (if-let [e (:error @state)] (assoc r :error e) r)]
                   (swap! state assoc :close r)
                   (doseq [[_ d] (:pings @state)]
                     (p/reject d (util/ws-error "connection closed" {:code :closed :conn-id (:id conn)})))
                   (swap! state assoc :pings {})
                   (async/close! in)
                   (async/close! out-ch)
                   (when on-close (on-close conn r))
                   (emit (merge {:event :close :conn conn} r))
                   (p/resolve (:closed @state) r))))
    (pump-out! conn (fn [e] (emit {:event :error :conn conn :error e})))
    conn))

;; ---------------------------------------------------------------------------
;; Registry, topics, broadcast
;; ---------------------------------------------------------------------------

(defn connections
  "The ws-server's core.async channel of connection events: `{:event :open
   :conn c}`, `{:event :close :conn c :code n :reason s :forced? b}`,
   `{:event :error :conn c :error e}`. Sliding buffer of 256 (consuming is
   optional; for lossless open/close hooks use `on-open!` / `on-close!`).
   Closed by `stop!`."
  [ws-server]
  (:connections ws-server))

(defn conns
  "Vector of the currently registered (accepted, not yet closed)
   connection maps."
  [ws-server]
  (vec (vals (:conns @(:state ws-server)))))

(defn conn-count
  "Number of registered connections."
  [ws-server]
  (count (:conns @(:state ws-server))))

(defn conn-by-id
  "The connection map with this :id, or nil."
  [ws-server id]
  (get-in @(:state ws-server) [:conns id]))

(defn on-open!
  "Registers `(fn [conn])`, called synchronously for every accepted
   connection (lossless, unlike the channel). Returns a zero-arity
   unsubscribe fn."
  [ws-server f]
  (let [k (str (random-uuid))]
    (swap! (:state ws-server) assoc-in [:on-open k] f)
    (fn [] (swap! (:state ws-server) update :on-open dissoc k))))

(defn on-close!
  "Registers `(fn [conn {:keys [code reason forced?]}])`, called for every
   closed connection after it left the registry and its topics. Returns
   an unsubscribe fn."
  [ws-server f]
  (let [k (str (random-uuid))]
    (swap! (:state ws-server) assoc-in [:on-close k] f)
    (fn [] (swap! (:state ws-server) update :on-close dissoc k))))

(defn- topic-name
  [topic]
  (cond (keyword? topic) (name topic) (string? topic) topic
        :else (throw (util/argument-error "topic must be a string or keyword" {:arg :topic :value topic}))))

(defn join!
  "Adds the connection to `topic` (a string or keyword; rooms are a
   wrapper-level concept, inventory 6.10). Returns the topic's member
   count. A connection leaves all its topics when it closes."
  [ws-server conn topic]
  (let [t (topic-name topic)
        st (swap! (:state ws-server)
                  (fn [s] (if (contains? (:conns s) (:id conn))
                            (-> s (update-in [:topics t] (fnil conj #{}) (:id conn))
                                (update-in [:conn-topics (:id conn)] (fnil conj #{}) t))
                            s)))]
    (count (get-in st [:topics t]))))

(defn leave!
  "Removes the connection from `topic`. Returns the remaining member count."
  [ws-server conn topic]
  (let [t (topic-name topic)
        st (swap! (:state ws-server)
                  (fn [s]
                    (let [s (-> s (update-in [:topics t] disj (:id conn))
                                (update-in [:conn-topics (:id conn)] disj t))]
                      (if (empty? (get-in s [:topics t])) (update s :topics dissoc t) s))))]
    (count (get-in st [:topics t]))))

(defn topics
  "The set of topic names a connection has joined (or, with one argument,
   every topic that has members)."
  ([ws-server] (set (keys (:topics @(:state ws-server)))))
  ([ws-server conn] (or (get-in @(:state ws-server) [:conn-topics (:id conn)]) #{})))

(defn topic->conns
  "Vector of the connection maps in `topic`."
  [ws-server topic]
  (let [s @(:state ws-server)]
    (into [] (keep #(get-in s [:conns %])) (get-in s [:topics (topic-name topic)]))))

(defn- target-conns
  [ws-server target]
  (cond
    (= :all target) (conns ws-server)
    (or (string? target) (keyword? target)) (topic->conns ws-server target)
    (map? target) [target]
    (coll? target) (vec target)
    :else (throw (util/argument-error "broadcast target must be :all, a topic, a connection or a collection of connections"
                                      {:arg :target :value target}))))

(defn broadcast!
  "Sends `data` (any `send!` payload) to every open connection (`:all` or
   the 2-arity), to a topic (string / keyword), or to a collection of
   connection maps. Option :except excludes one connection (the sender).
   Resolves `{:sent n :failed n :errors [..]}` — a closed peer never
   rejects the broadcast (inventory 6.10)."
  ([ws-server data] (broadcast! ws-server :all data {}))
  ([ws-server target data] (broadcast! ws-server target data {}))
  ([ws-server target data {:keys [except]}]
   (let [cs (cond->> (target-conns ws-server target)
              except (remove #(= (:id %) (:id except))))
         cs (filter open? cs)]
     (p/let [rs (p/all (map (fn [c] (-> (send! c data)
                                        (p/then (fn [_] nil))
                                        (p/catch (fn [e] e))))
                            cs))]
       (let [errors (vec (remove nil? rs))]
         {:sent (- (count rs) (count errors))
          :failed (count errors)
          :errors errors})))))

(defn emit!
  "`(broadcast! ws-server topic data opts)` — the topic-oriented spelling."
  ([ws-server topic data] (broadcast! ws-server topic data {}))
  ([ws-server topic data opts] (broadcast! ws-server topic data opts)))

;; ---------------------------------------------------------------------------
;; Upgrade handling (inventory 2.3, 6.9)
;; ---------------------------------------------------------------------------

(defn- pathname
  [url]
  (first (str/split (str url) #"\?" 2)))

(defn- abort-upgrade!
  "Answers a raw HTTP response on the not-yet-upgraded socket and closes
   it (the pattern ws itself uses in abortHandshake)."
  [socket status message headers]
  (when-not (jsget socket "destroyed")
    (if (jsget socket "writable")
      (let [body (str status " " message)
            lines (concat [(str "HTTP/1.1 " status " " (util/reason-phrase status))
                           "Connection: close"
                           "Content-Type: text/plain; charset=utf-8"
                           (str "Content-Length: " (js-invoke js/Buffer "byteLength" body "utf8"))]
                          (map (fn [[k v]] (str k ": " (if (vector? v) (str/join ", " v) v)))
                               (util/->wire-headers headers)))]
        (js-invoke socket "once" "finish" (fn [] (js-invoke socket "destroy")))
        (js-invoke socket "end" (str (str/join "\r\n" lines) "\r\n\r\n" body)))
      (js-invoke socket "destroy"))))

(defn- upgrade-registry
  "The per-http.Server atom of upgrade routes `[{:key :match :handle}]`
   behind the ONE Node 'upgrade' listener (created on first use): the
   first route whose :match accepts the request handles it; without a
   match the socket gets a 404 (Node accepts every upgrade once a
   listener exists, inventory 2.3)."
  [raw]
  (or (jsget raw upgrade-key)
      (let [reg (atom [])]
        (jsset! raw upgrade-key reg)
        (js-invoke raw "on" "upgrade"
                   (fn [req socket head]
                     (if-let [r (some (fn [r] (when ((:match r) req) r)) @reg)]
                       ((:handle r) req socket head)
                       (abort-upgrade! socket 404 "no websocket endpoint at this path" {}))))
        reg)))

(defn- path-matcher
  [path]
  (cond
    (string? path) (fn [req] (= path (pathname (jsget req "url"))))
    (set? path) (fn [req] (contains? path (pathname (jsget req "url"))))
    (fn? path) (fn [req] (boolean (path (pathname (jsget req "url")))))
    :else (throw (util/argument-error ":path must be a string, a set of strings or a predicate of the pathname"
                                      {:arg :path :value path}))))

(defn- auth-failure->status
  [e]
  (let [d (or (util/error-data e) {})]
    (case (:type d)
      :express-cljs/http-error [(:status d) (if (:expose? d) (:message d) (util/reason-phrase (:status d))) (:headers d)]
      [500 "Internal Server Error" {}])))

(defn- protocol-selector
  "`:protocols` option -> ws handleProtocols fn (inventory 6.9): a vector
   of accepted names picks the first client-offered one that is
   accepted; a fn receives (offered-vector upgrade-map) and returns a
   name or nil."
  [protocols app]
  (cond
    (nil? protocols) nil
    (sequential? protocols)
    (let [accepted (set (map str protocols))]
      (fn [offered _req]
        (or (some (fn [o] (when (contains? accepted o) o)) (array-seq (js-invoke js/Array "from" offered))) false)))
    (fn? protocols)
    (fn [offered req]
      (or (protocols (vec (array-seq (js-invoke js/Array "from" offered))) (core/upgrade-request->map app req)) false))
    :else (throw (util/argument-error ":protocols must be a vector of names or a function" {:arg :protocols :value protocols}))))

(def serve-option-keys
  "serve! options (kebab, `?` stripped): :path (string, set of strings or
   pathname predicate; default \"/ws\"), :max-payload (bytes, default 1 MiB,
   pitfall 9.10), :keepalive-ms (default 30000; false disables),
   :authenticate (fn [upgrade-map] -> ctx | promise | nil), :protocols
   (vector or fn, inventory 6.9), :handshake-headers (fn [upgrade-map] ->
   header map added to the 101 response), :per-message-deflate (boolean or
   option map), :close-timeout-ms (the wrapper's per-socket deadline,
   default 2000), :max-buffered-bytes (send backpressure limit, default
   8 MiB), :message-buffer (IN channel size, default 64), :decode (:json
   parses text frames eagerly), :max-buffered-chunks / :max-fragments /
   :allow-synchronous-events / :auto-pong (ws passthrough), :shutdown-code
   / :shutdown-reason (default 1001 \"server shutdown\")."
  #{:path :max-payload :keepalive-ms :authenticate :protocols :handshake-headers
    :per-message-deflate :close-timeout-ms :max-buffered-bytes :message-buffer :decode
    :max-buffered-chunks :max-fragments :allow-synchronous-events :auto-pong
    :shutdown-code :shutdown-reason})

(def ^:private serve-defaults
  {:path "/ws" :max-payload 1048576 :keepalive-ms 30000 :close-timeout-ms 2000
   :max-buffered-bytes 8388608 :message-buffer 64 :shutdown-code 1001 :shutdown-reason "server shutdown"})

(defn- wss-options
  [opts app]
  (let [passthrough (select-keys opts [:max-payload :per-message-deflate :max-buffered-chunks :max-fragments
                                       :allow-synchronous-events :auto-pong])
        o (cond-> (assoc passthrough :no-server true :client-tracking true)
            (map? (:per-message-deflate opts)) (assoc :per-message-deflate (:per-message-deflate opts)))
        js-opts (clj->js (util/->camel-keys o))]
    (when-let [sel (protocol-selector (:protocols opts) app)]
      (jsset! js-opts "handleProtocols" sel))
    js-opts))

(defn- emit-event!
  [ws-server ev]
  (async/put! (:connections ws-server) ev))

(defn- register-conn!
  [ws-server conn]
  (swap! (:state ws-server) assoc-in [:conns (:id conn)] conn)
  (doseq [[_ f] (:on-open @(:state ws-server))]
    (try (f conn) (catch :default e (emit-event! ws-server {:event :error :conn conn :error (core/->error e)}))))
  (emit-event! ws-server {:event :open :conn conn}))

(defn- unregister-conn!
  [ws-server conn close-info]
  (swap! (:state ws-server)
         (fn [s]
           (let [ts (get-in s [:conn-topics (:id conn)])]
             (-> (reduce (fn [s t]
                           (let [s (update-in s [:topics t] disj (:id conn))]
                             (if (empty? (get-in s [:topics t])) (update s :topics dissoc t) s)))
                         s ts)
                 (update :conns dissoc (:id conn))
                 (update :conn-topics dissoc (:id conn))))))
  (doseq [[_ f] (:on-close @(:state ws-server))]
    (try (f conn close-info) (catch :default e (emit-event! ws-server {:event :error :conn conn :error (core/->error e)})))))

(defn- accept!
  "handleUpgrade + connection map + registry."
  [ws-server req socket head upgrade-map ctx]
  (let [wss (:wss ws-server)
        opts (:opts ws-server)]
    (js-invoke wss "handleUpgrade" req socket head
               (fn [ws]
                 (let [sub (jsget ws "protocol")
                       fields {:id (str (random-uuid))
                               :ctx (if (map? ctx) ctx {})
                               :remote-addr (:remote-address upgrade-map)
                               :remote-port (:remote-port upgrade-map)
                               :ip (:ip upgrade-map)
                               :headers (:headers upgrade-map)
                               :protocol (if (:secure? upgrade-map) :wss :ws)
                               :subprotocol (when-not (str/blank? sub) sub)
                               :upgrade upgrade-map
                               :server-id (:id ws-server)}
                       conn (wrap-socket! ws fields opts
                                          {:emit (fn [ev] (emit-event! ws-server ev))
                                           :on-close (fn [conn r] (unregister-conn! ws-server conn r))})]
                   (register-conn! ws-server conn)
                   (js-invoke wss "emit" "connection" ws req))))))

(defn- handle-upgrade!
  "The per-serve! upgrade handler: request map, authenticate, accept or
   answer a real HTTP error before the handshake (inventory 2.3)."
  [ws-server req socket head]
  (let [app (:app (:server ws-server))
        {:keys [authenticate]} (:opts ws-server)]
    (if (:closing? @(:state ws-server))
      (abort-upgrade! socket 503 "server is shutting down" {:retry-after "1"})
      (let [upgrade-map (try (core/upgrade-request->map app req) (catch :default e e))]
        (if (some? (ex-data upgrade-map))
          (abort-upgrade! socket 400 "malformed upgrade request" {})
          (-> (p/do (if authenticate (authenticate upgrade-map) {}))
              (p/then (fn [ctx]
                        (if ctx
                          (accept! ws-server req socket head upgrade-map (if (map? ctx) ctx {}))
                          (abort-upgrade! socket 401 "websocket authentication required" {}))))
              (p/catch (fn [e]
                         (let [[status message headers] (auth-failure->status (core/->error e))]
                           (emit-event! ws-server {:event :error :conn nil :error (core/->error e) :upgrade upgrade-map})
                           (abort-upgrade! socket status message headers))))))))))

;; ---------------------------------------------------------------------------
;; Keepalive (inventory 6.7)
;; ---------------------------------------------------------------------------

(defn- keepalive-tick!
  [ws-server]
  (doseq [conn (conns ws-server)]
    (let [{:keys [ws state]} (raw-of conn)]
      (when (= 1 (jsget ws "readyState"))
        (if (false? (:alive @state))
          (do (swap! state assoc :forced? true :keepalive-terminated? true)
              (swap! (:state ws-server) update :keepalive-terminated (fnil inc 0))
              (js-invoke ws "terminate"))
          (do (swap! state assoc :alive false)
              (swap! (:state ws-server) update :keepalive-pings (fnil inc 0))
              (js-invoke ws "ping")))))))

(defn- start-keepalive!
  "One unref'd setInterval per ws-server (never keeps the process alive,
   liveness precedent)."
  [ws-server ms]
  (when (and (number? ms) (pos? ms))
    (let [t (js/setInterval (fn [] (keepalive-tick! ws-server)) ms)]
      (js-invoke t "unref")
      (swap! (:state ws-server) assoc :keepalive t)
      t)))

(defn keepalive-stats
  "{:pings n :terminated n} — pings sent by the keepalive interval and
   peers terminated for not answering (test / monitoring evidence)."
  [ws-server]
  (let [s @(:state ws-server)]
    {:pings (or (:keepalive-pings s) 0) :terminated (or (:keepalive-terminated s) 0)}))

;; ---------------------------------------------------------------------------
;; serve! / stop!
;; ---------------------------------------------------------------------------

(declare stop!)

(defn- close-wss!
  "wss.close(cb) with a deadline: ws only calls back once every tracked
   client is gone, so a straggler accepted mid-shutdown must not hang
   `core/close!`."
  [wss timeout-ms]
  (p/create
   (fn [resolve _]
     (let [done (atom false)
           finish (fn [] (when (compare-and-set! done false true) (resolve nil)))
           timer (js/setTimeout finish (+ timeout-ms 100))]
       (js-invoke timer "unref")
       (js-invoke wss "close" (fn [_] (js/clearTimeout timer) (finish)))))))

(defn serve!
  "Attaches a websocket endpoint to a listening `core/listen!` server
   handle and returns the ws-server value (synchronously). Options: see
   `serve-option-keys`. A connection is accepted when the upgrade
   pathname matches :path and `:authenticate` (given the upgrade request
   map from `core/upgrade-request->map`) returns a truthy ctx (a map is
   stored as the connection's :ctx; a promise is awaited); nil / false
   answers HTTP 401 before the handshake, a thrown http-error answers its
   status, anything else 500. Registers a close hook so `core/close!`
   closes every connection with :shutdown-code and detaches (see
   `stop!`). Argument-error when the same path is already served on this
   server."
  ([server] (serve! server {}))
  ([server opts]
   (let [opts (merge serve-defaults (util/check-opts! :serve serve-option-keys (util/normalize-opts opts)))
         raw (:server server)
         _ (when (nil? raw) (throw (util/argument-error "serve!: expected a server handle from core/listen!" {:arg :server})))
         reg (upgrade-registry raw)
         key (str (random-uuid))
         path (:path opts)
         _ (when (and (string? path) (some #(= path (:path %)) @reg))
             (throw (util/argument-error (str "serve!: path " (pr-str path) " is already served on this server") {:arg :path :value path})))
         wss (let [ctor (jsget ws-module "WebSocketServer")]
               (new ctor (wss-options opts (:app server))))
         ws-server {:wss wss
                    :server server
                    :opts opts
                    :path path
                    :id key
                    :state (atom {:conns {} :topics {} :conn-topics {} :on-open {} :on-close {} :closing? false})
                    :connections (async/chan (async/sliding-buffer 256))}]
     (js-invoke wss "on" "error" (fn [err] (emit-event! ws-server {:event :error :conn nil :error (core/->error err)})))
     (js-invoke wss "on" "wsClientError"
                (fn [err socket _req]
                  (emit-event! ws-server {:event :error :conn nil :error (core/->error err)})
                  (abort-upgrade! socket 400 (str (jsget err "message")) {})))
     (when-let [hh (:handshake-headers opts)]
       (js-invoke wss "on" "headers"
                  (fn [headers req]
                    (doseq [[k v] (util/->wire-headers (hh (core/upgrade-request->map (:app server) req)))]
                      (js-invoke headers "push" (str k ": " (if (vector? v) (str/join ", " v) v)))))))
     (swap! reg conj {:key key :path path
                      :match (path-matcher path)
                      :handle (fn [req socket head] (handle-upgrade! ws-server req socket head))})
     (start-keepalive! ws-server (:keepalive-ms opts))
     (core/add-close-hook! server (fn [{:keys [timeout-ms]}] (stop! ws-server {:timeout-ms timeout-ms})))
     ws-server)))

(defn stop!
  "Shuts the websocket endpoint down (inventory 5.3 choreography, run by
   `core/close!` through the close hook — call it directly only to stop
   the endpoint while the HTTP server keeps running): stops accepting
   upgrades (503), clears the keepalive interval, sends `close(1001,
   \"server shutdown\")` to every connection and awaits each 'close' with
   the :timeout-ms deadline (default :close-timeout-ms) before
   `terminate()`, then `wss.close()` and closes the connections channel.
   Resolves `{:closed n :terminated n}`; idempotent (a second call returns
   the same promise)."
  ([ws-server] (stop! ws-server {}))
  ([ws-server {:keys [timeout-ms]}]
   (let [st (:state ws-server)]
     (if-let [done (:stopping @st)]
       done
       (let [{:keys [shutdown-code shutdown-reason close-timeout-ms]} (:opts ws-server)
             timeout-ms (or timeout-ms close-timeout-ms)
             raw (:server (:server ws-server))
             done
             (p/let [_ (do (swap! st assoc :closing? true)
                           (when-let [reg (jsget raw upgrade-key)]
                             (swap! reg (fn [rs] (vec (remove #(= (:id ws-server) (:key %)) rs)))))
                           (when-let [t (:keepalive @st)] (js/clearInterval t))
                           nil)
                     results (p/all (map (fn [c] (close! c shutdown-code shutdown-reason {:timeout-ms timeout-ms}))
                                         (conns ws-server)))
                     _ (close-wss! (:wss ws-server) timeout-ms)]
               (async/close! (:connections ws-server))
               {:closed (count results)
                :terminated (count (filter :forced? results))})]
         (swap! st assoc :stopping done)
         done)))))

;; ---------------------------------------------------------------------------
;; Client side (tests, smoke, service-to-service)
;; ---------------------------------------------------------------------------

(def connect-option-keys
  "connect! options: :headers (kebab map sent with the handshake),
   :protocols (vector of subprotocol names to offer), :max-payload,
   :timeout-ms (handshake deadline, default 5000), :message-buffer,
   :decode (:json), :max-buffered-bytes, :close-timeout-ms,
   :per-message-deflate."
  #{:headers :protocols :max-payload :timeout-ms :message-buffer :decode
    :max-buffered-bytes :close-timeout-ms :per-message-deflate})

(defn events
  "A CLIENT connection's channel of `{:event :close ..}` / `{:event :error
   ..}` (server-side connections report on the ws-server's `connections`
   channel instead)."
  [conn]
  (:events conn))

(defn connect!
  "Opens a websocket to `url` (ws:// or wss://) with the `ws` client and
   resolves with a connection map that works with `send!`, `messages`,
   `out`, `ping!`, `close!`, `terminate!`, `closed` exactly like a
   server-side one (:client? true, :url, :subprotocol, :handshake-headers
   — the 101 response headers, kebab —, :events channel).
   Rejects with a ws-error {:code :unexpected-response :status 401 ..}
   when the server answered the handshake with an HTTP error, {:code
   :timeout} after :timeout-ms, or the socket error otherwise."
  ([url] (connect! url {}))
  ([url opts]
   (let [opts (util/check-opts! :connect connect-option-keys (util/normalize-opts opts))
         {:keys [headers protocols max-payload timeout-ms] :or {timeout-ms 5000}} opts
         js-opts (clj->js (cond-> {}
                            headers (assoc "headers" (util/->wire-headers headers))
                            max-payload (assoc "maxPayload" max-payload)
                            (contains? opts :per-message-deflate) (assoc "perMessageDeflate" (:per-message-deflate opts))))
         ctor (jsget ws-module "WebSocket")
         ws (if (seq protocols) (new ctor url (to-array protocols) js-opts) (new ctor url js-opts))
         events (async/chan (async/sliding-buffer 64))
         handshake (atom {})
         settled (atom false)]
     (p/create
      (fn [resolve reject]
        (let [timer (js/setTimeout (fn []
                                     (when (compare-and-set! settled false true)
                                       (js-invoke ws "terminate")
                                       (reject (util/ws-error "websocket handshake timed out" {:code :timeout :url url :timeout-ms timeout-ms}))))
                                   timeout-ms)
              conn (wrap-socket! ws {:id (str (random-uuid)) :client? true :url url :ctx {}}
                                 opts
                                 {:emit (fn [ev] (async/put! events ev))})]
          (js-invoke timer "unref")
          (js-invoke ws "once" "upgrade"
                     (fn [res] (reset! handshake (util/->header-map (->clj* (jsget res "headers"))))))
          (js-invoke ws "once" "open"
                     (fn []
                       (js/clearTimeout timer)
                       (when (compare-and-set! settled false true)
                         (let [sub (jsget ws "protocol")]
                           (resolve (assoc conn :subprotocol (when-not (str/blank? sub) sub) :events events
                                           :handshake-headers @handshake
                                           :opened-at (js/Date.now)))))))
          (js-invoke ws "once" "error"
                     (fn [err]
                       (js/clearTimeout timer)
                       (when (compare-and-set! settled false true)
                         (let [msg (str (jsget err "message"))
                               [_ status] (re-find #"Unexpected server response: (\d+)" msg)]
                           (reject (if status
                                     (util/ws-error msg {:code :unexpected-response :status (js/parseInt status 10) :url url :cause err})
                                     (->ws-error err conn)))))))))))))
