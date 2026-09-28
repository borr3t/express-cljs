(ns express-cljs.middleware
  "Middleware helpers. Pure ClojureScript, no interop.

   Two kinds live here:

   - DESCRIPTORS for Express's built-in middleware — `json`, `urlencoded`,
     `text`, `raw` (body-parser 2.3) and `static` (serve-static 2.2). They
     are data (`{:express-cljs/builtin :json :opts {...}}`) validated
     against `parser-option-keys` / `static-option-keys`; the binding
     instantiates the upstream middleware when the plan is registered.
     Put them in a `:use` slot, a route chain, or `core/app` options.

   - WRAPPERS in the Ring style, `(wrap-x handler opts) -> handler`, which
     compose over the `(fn [req] -> response | nil | promise)` contract:
     `wrap-json`, `wrap-logging`, `wrap-errors->http-error`,
     `wrap-require-header`, `wrap-timing`, `wrap-cors`, `wrap-bearer-auth`,
     plus `compose` and `wrap-when`. Wrappers never see `req` / `res`; they
     transform request maps and response maps.

   Express-layer helpers that continue instead of answering (`request-id`)
   return `route/continue` values. Wrappers report their own failures as
   REJECTED promises (never synchronous throws), so they compose with
   `p/let` and `tu/rejection`-style callers alike."
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.route :as route]
            [express-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Built-in middleware descriptors
;; ---------------------------------------------------------------------------

(def body-parser-option-keys
  "Options every body parser accepts (kebab, `?` stripped): :limit
   (\"100kb\" default; bytes string or number), :inflate? (default true),
   :type (string / vector / fn of the raw Node request — passthrough),
   :verify (fn — passthrough, receives Node objects)."
  #{:limit :inflate :type :verify})

(def parser-option-keys
  "Per-parser option whitelists."
  {:json (into body-parser-option-keys #{:strict :reviver :default-charset})
   :urlencoded (into body-parser-option-keys #{:extended :parameter-limit :depth :default-charset
                                              :charset-sentinel :interpret-numeric-entities})
   :text (into body-parser-option-keys #{:default-charset})
   :raw body-parser-option-keys})

(defn- builtin
  [kind opts]
  (let [opts (util/check-opts! kind (parser-option-keys kind) (util/normalize-opts opts))]
    {:express-cljs/builtin kind :opts opts}))

(defn json
  "express.json descriptor: parses `application/json` bodies into :body
   (keys keywordised as spelled, never re-cased). Options: :limit
   (\"100kb\"), :strict? (true: only objects/arrays), :inflate?, :type,
   :verify, :reviver, :default-charset. Malformed JSON -> 400
   :parse-failed; over :limit -> 413 :too-large; a non-matching
   Content-Type is skipped silently (:body stays nil)."
  ([] (json {}))
  ([opts] (builtin :json opts)))

(defn urlencoded
  "express.urlencoded descriptor: `application/x-www-form-urlencoded` ->
   :body map. Options: :limit, :extended? (default false: flat keys;
   true nests `a[b]` via qs with allowPrototypes),
   :parameter-limit (1000), :depth (32), :inflate?, :type, :verify,
   :default-charset, :charset-sentinel?, :interpret-numeric-entities?."
  ([] (urlencoded {}))
  ([opts] (builtin :urlencoded opts)))

(defn text
  "express.text descriptor: `text/plain` -> :body string. Options: :limit,
   :default-charset, :inflate?, :type, :verify."
  ([] (text {}))
  ([opts] (builtin :text opts)))

(defn raw
  "express.raw descriptor: `application/octet-stream` -> :body Buffer
   (opaque). Options: :limit, :inflate?, :type, :verify."
  ([] (raw {}))
  ([opts] (builtin :raw opts)))

(def static-option-keys
  "express.static options: :accept-ranges?, :cache-control?,
   :dotfiles (:allow / :deny / :ignore), :etag?, :extensions (vector),
   :fallthrough? (default true), :immutable?, :index (string / vector /
   false), :last-modified?, :max-age (ms or \"1d\"), :redirect?,
   :set-headers (fn [path stat-map] -> header map)."
  #{:accept-ranges :cache-control :dotfiles :etag :extensions :fallthrough
    :immutable :index :last-modified :max-age :redirect :set-headers})

(defn static
  "express.static descriptor serving files below `root` (an absolute
   directory; traversal above it is refused upstream).
   Mount it with a path: `(route/use \"/static\" (static \"/srv/pub\"))` or
   `(route/static \"/static\" \"/srv/pub\" opts)`. Defaults: dotfiles
   ignored (404), `index.html` served for directories, weak ETag,
   `Cache-Control: public, max-age=0`, misses fall through to the next
   handler (the wrapper's 404)."
  ([root] (static root {}))
  ([root opts]
   (when-not (and (string? root) (not (str/blank? root)))
     (throw (util/argument-error "static: root must be a non-empty directory path" {:arg :root :value root})))
   (let [opts (util/check-opts! :static static-option-keys (util/normalize-opts opts))]
     (when (and (contains? opts :dotfiles) (not (contains? #{:allow :deny :ignore} (:dotfiles opts))))
       (throw (util/argument-error (str "static: :dotfiles must be :allow, :deny or :ignore, got " (pr-str (:dotfiles opts)))
                                   {:arg :dotfiles :value (:dotfiles opts)})))
     {:express-cljs/builtin :static :root root :opts opts})))

;; ---------------------------------------------------------------------------
;; Express-layer helpers (continue semantics)
;; ---------------------------------------------------------------------------

(defn request-id
  "Middleware that gives every request an id: reuses the incoming header
   (default `x-request-id`) when present, else a random UUID; the id is
   merged into the request map as :request-id and echoed as a response
   header. Options: :header (keyword, default :x-request-id)."
  ([] (request-id {}))
  ([{:keys [header] :or {header :x-request-id}}]
   (fn [req]
     (let [id (or (not-empty (str (get-in req [:headers header])))
                  (str (random-uuid)))]
       (route/continue {:request-id id} {header id})))))

;; ---------------------------------------------------------------------------
;; Ring-style wrappers
;; ---------------------------------------------------------------------------

(defn compose
  "Composes wrappers right-to-left like `comp`: `((compose wrap-a wrap-b)
   handler)` is `(wrap-a (wrap-b handler))`, so wrap-a runs outermost."
  [& wrappers]
  (fn [handler] (reduce (fn [h w] (w h)) handler (reverse wrappers))))

(defn wrap-when
  "Applies `wrapper` only to requests satisfying `pred` (a fn of the request
   map); other requests reach `handler` untouched."
  [handler pred wrapper]
  (let [wrapped (wrapper handler)]
    (fn [req] (if (pred req) (wrapped req) (handler req)))))

(defn wrap-json
  "Forces JSON: every non-nil, non-control response body is rendered with
   `res.json` (`:json true`; strings included), and a request that carries
   a body without a JSON Content-Type is refused with 415 before the
   handler runs. Option :require-json-body? (default true)."
  ([handler] (wrap-json handler {}))
  ([handler {:keys [require-json-body?] :or {require-json-body? true}}]
   (fn [req]
     (let [ct (str (:content-type req))
           has-body? (some-> (:content-length req) pos?)]
       (if (and require-json-body? has-body? (not (str/includes? ct "json")))
         (p/rejected (util/http-error 415 "expected a JSON body" {:reason :unsupported-media-type}))
         (p/let [r (handler req)]
           (if (and (map? r) (not (route/control? r)) (contains? r :body) (some? (:body r)))
             (assoc r :json true)
             r)))))))

(defn wrap-timing
  "Adds an `x-response-time` header (\"12.3ms\") to map responses; nil /
   control values pass through untouched."
  [handler]
  (fn [req]
    (let [start (system-time)]
      (p/let [r (handler req)]
        (if (and (map? r) (not (route/control? r)))
          (assoc-in r [:headers :x-response-time]
                    (str (/ (int (* 10 (- (system-time) start))) 10) "ms"))
          r)))))

(defn wrap-logging
  "Logs one line per handled request through `:log` (a fn of a map, default
   println of {:method :path :status :ms}); errors are logged with :error
   and re-thrown. nil / control results log :status :next."
  ([handler] (wrap-logging handler {}))
  ([handler {:keys [log] :or {log println}}]
   (fn [req]
     (let [start (system-time)
           base {:method (:method req) :path (:original-url req)}
           ms #(/ (int (* 10 (- (system-time) start))) 10)]
       (-> (p/do (handler req))
           (p/then (fn [r]
                     (log (assoc base :status (if (and (map? r) (not (route/control? r)))
                                                (clojure.core/get r :status 200)
                                                :next)
                                 :ms (ms)))
                     r))
           (p/catch (fn [e]
                      (log (assoc base :status (util/error-status e) :error (ex-message e) :ms (ms)))
                      (throw e))))))))

(defn wrap-errors->http-error
  "Converts every non-http error escaping `handler` into an http-error so
   the client only ever sees the wrapper's error shape with a controlled
   message: argument-errors become 400 with their message, anything else
   `:status` (default 500) with `:message` (default the reason phrase);
   the original error travels as :cause. http-errors pass through."
  ([handler] (wrap-errors->http-error handler {}))
  ([handler {:keys [status message] :or {status 500}}]
   (fn [req]
     (-> (p/do (handler req))
         (p/catch (fn [e]
                    (case (util/error-type e)
                      :express-cljs/http-error (throw e)
                      :express-cljs/argument-error (throw (util/http-error 400 (ex-message e) {:cause e}))
                      (throw (util/http-error status message {:cause e})))))))))

(defn wrap-require-header
  "Answers 400 (or `:status`) when request header `header` (kebab keyword)
   is absent or, when `pred` is given, fails it."
  ([handler header] (wrap-require-header handler header {}))
  ([handler header {:keys [pred status message] :or {status 400}}]
   (fn [req]
     (let [v (get-in req [:headers header])]
       (if (or (nil? v) (and pred (not (pred v))))
         (p/rejected (util/http-error status (or message (str "missing or invalid header " (util/header-name header)))
                                      {:reason :missing-header :header header}))
         (handler req))))))

(defn wrap-bearer-auth
  "Bearer-token authentication: `:verify` is `(fn [token] -> ctx-map | nil
   | promise)`; a nil result or a missing `Authorization: Bearer ..` header
   answers 401 with `WWW-Authenticate: Bearer realm=\"..\"` (:realm
   default \"api\"). On success the ctx map is merged into the request the
   handler receives (as :auth by default, or under :key)."
  [handler {:keys [verify realm key] :or {realm "api" key :auth}}]
  (when-not (fn? verify)
    (throw (util/argument-error "wrap-bearer-auth: :verify must be a function" {:arg :verify})))
  (let [challenge {:www-authenticate (str "Bearer realm=\"" realm "\"")}
        unauthorized (fn [msg] (util/http-error 401 msg {:headers challenge :reason :unauthorized}))]
    (fn [req]
      (let [auth (str (get-in req [:headers :authorization]))
            [_ token] (re-matches #"(?i)bearer\s+(\S+)\s*" auth)]
        (if-not token
          (p/rejected (unauthorized "bearer token required"))
          (p/let [ctx (verify token)]
            (if ctx
              (handler (assoc req key ctx))
              (throw (unauthorized "invalid token")))))))))

(defn wrap-cors
  "Adds CORS headers as data: :origins (vector of allowed origins or
   [\"*\"], default [\"*\"]), :methods (vector of keywords/strings, default
   [:get :post :put :patch :delete :options]), :headers (allowed request
   headers, default [\"content-type\" \"authorization\"]), :credentials?
   (adds Access-Control-Allow-Credentials), :max-age (preflight cache
   seconds). A preflight (OPTIONS with Access-Control-Request-Method) is
   answered 204 without calling the handler. Requests from an origin not in
   :origins get no CORS headers (the browser blocks them)."
  ([handler] (wrap-cors handler {}))
  ([handler {:keys [origins methods headers credentials? max-age]
             :or {origins ["*"]
                  methods [:get :post :put :patch :delete :options]
                  headers ["content-type" "authorization"]}}]
   (let [render (fn [xs] (str/join ", " (map #(if (keyword? %) (str/upper-case (name %)) (str %)) xs)))
         any? (some #{"*"} origins)
         cors-headers (fn [origin]
                        (when (and origin (or any? (some #{origin} origins)))
                          (cond-> {:access-control-allow-origin (if (and any? (not credentials?)) "*" origin)
                                   :vary "Origin"}
                            credentials? (assoc :access-control-allow-credentials "true"))))]
     (fn [req]
       (let [origin (get-in req [:headers :origin])
             hs (cors-headers origin)
             preflight? (and (= :options (:method req))
                             (some? (get-in req [:headers :access-control-request-method])))]
         (if preflight?
           {:status 204
            :headers (cond-> (merge hs {:access-control-allow-methods (render methods)
                                        :access-control-allow-headers (render headers)})
                       max-age (assoc :access-control-max-age (str max-age)))
            :end true}
           (p/let [r (handler req)]
             (if (and hs (map? r) (not (route/control? r)))
               (update r :headers #(merge hs %))
               r))))))))
