(ns express-cljs.core
  "The ONE binding namespace: every line of Express / Node interop in the
   library lives here (inventory §11.2). Nothing else in express-cljs, and
   nothing at a call site, touches an Express `app`, a Node `req` / `res`,
   an `http.Server` or a stream. House style: `js-invoke` + `goog.object`,
   no `^js` hints, no dot-forms.

   Values this namespace hands out:

   - an APP value from `app`: `{:express <opaque express app> :opts .. :env
     .. :plan [..] :state <atom>}`; pass it to `listen!`, `mount-upstream!`,
     `setting` / `set-setting!`.
   - a SERVER handle from `listen!`: `{:server <opaque http.Server> :app app
     :address {:address :family :port} :events <core.async chan> :close-hooks
     <atom>}`; pass it to `close!`, `address`, `listening?`, `connections!`,
     `events`, `add-close-hook!`.

   Handlers are `(fn [req-map] -> response-map | nil | control | promise)`
   (inventory 11.3 / 11.4): the request map is built by `req->map`, the
   response map is executed by `respond!`; nil means next(); a rejection or
   throw goes to the error chain as an ex-info (`->error`), and the
   wrapper's final error handler renders `util/error-response` (inventory
   8.5). Streaming: a `:body` that is a Node Readable (see `readable-from`)
   is piped (uncertainty 10.8).

   Cookies: the `Cookie` header is parsed here (RFC 6265 pair split +
   decodeURIComponent, mirroring the `cookie` package) into :cookies; with
   an app `:cookie-secret`, `s:`-prefixed values are verified with Node
   crypto (HMAC-SHA256, byte-compatible with `cookie-signature`,
   uncertainty 10.3) into :signed-cookies and `{:signed? true}` cookie
   entries are signed on the way out. No cookie-parser dependency."
  (:refer-clojure :exclude [range methods])
  (:require [clojure.string :as str]
            [cljs.core.async :as async]
            [goog.object :as gobj]
            [promesa.core :as p]
            [express-cljs.route :as route]
            [express-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Modules and the interop helpers
;; ---------------------------------------------------------------------------

(def ^:private express-module (js/require "express"))
(def ^:private node-http (js/require "http"))
(def ^:private node-crypto (js/require "crypto"))
(def ^:private node-stream (js/require "stream"))

(defn- jsget
  "Property read without interop syntax at the call site: (jsget o \"name\")
   or (jsget o \"a\" \"b\") for a path. nil-safe. Getters (Express's
   req.ip etc.) are invoked as a side effect of the read."
  [o & path]
  (when (some? o) (apply gobj/getValueByKeys o path)))

(defn- jsset!
  [o k v]
  (gobj/set o k v)
  o)

(def ^:private ctx-key "__expressCljsCtx")
(def ^:private base-key "__expressCljsBase")

(defn- plain-object?
  "A JS object that should become a CLJS map: null-prototype (req.params,
   querystring results) or Object-prototype. Buffers, Dates, streams and
   class instances are left opaque."
  [v]
  (and (some? v)
       (= "object" (goog/typeOf v))
       (let [proto (js-invoke js/Object "getPrototypeOf" v)]
         (or (nil? proto) (identical? proto (jsget js/Object "prototype"))))))

(defn- ->clj*
  "JS -> CLJS with keys keywordised AS SPELLED (never re-cased, inventory
   11.1); arrays -> vectors; opaque objects untouched."
  [v]
  (cond
    (array? v) (mapv ->clj* (array-seq v))
    (plain-object? v) (into {} (map (fn [k] [(keyword k) (->clj* (gobj/get v k))])) (js-keys v))
    :else v))

(defn- ->js*
  "CLJS -> JS for user data (bodies, links): keyword keys/values render as
   their name (no re-casing); opaque JS values pass through."
  [v]
  (clj->js v :keyword-fn name))

(defn buffer?
  "True for a Node Buffer or any Uint8Array (opaque binary body)."
  [v]
  (or (js-invoke js/Buffer "isBuffer" v) (instance? js/Uint8Array v)))

(defn readable?
  "True for a Node Readable stream (an opaque streaming :body, uncertainty
   10.8)."
  [v]
  (instance? (jsget node-stream "Readable") v))

(defn readable-from
  "A Node Readable over a seq of strings and/or Buffers (`Readable.from`),
   the interop-free way to build a streaming `:body` (each element is one
   chunk; strings are UTF-8)."
  [chunks]
  (js-invoke (jsget node-stream "Readable") "from" (to-array chunks)))

(def methods
  "The set of lower-case method keywords this Node exposes (http.METHODS,
   inventory 3.8); Express derives its app.METHOD functions from the same
   list, so this is the runtime truth `route/methods` approximates."
  (into #{} (map #(keyword (str/replace (str/lower-case %) "_" "-"))) (array-seq (jsget node-http "METHODS"))))

;; ---------------------------------------------------------------------------
;; Errors: anything thrown upstream or by a handler -> ex-info
;; ---------------------------------------------------------------------------

(defn ->error
  "Normalises any thrown / rejected value into an express-cljs ex-info
   (inventory 8.6): express-cljs errors and other ex-infos pass through; a
   JS Error with a 4xx/5xx `status` / `statusCode` (body-parser,
   serve-static, res.format http-errors) becomes an http-error carrying
   :reason (from `err.type`, inventory 4.1), :headers and the upstream
   :expose flag, with the original as :cause; other JS Errors become a
   server-error with the JS `code`; strings become server-errors with the
   string as message; a falsy rejection is {:code :rejected-falsy}."
  [e]
  (cond
    (some? (ex-data e)) e
    (instance? js/Error e)
    (let [status (or (jsget e "status") (jsget e "statusCode"))
          message (str (jsget e "message"))]
      (if (and (util/valid-status? status) (>= status 400))
        (util/http-error status message
                         (cond-> {:cause e
                                  :reason (util/upstream-reason (jsget e "type"))
                                  :expose? (if (some? (jsget e "expose")) (boolean (jsget e "expose")) (< status 500))}
                           (some? (jsget e "headers")) (assoc :headers (util/->header-map (->clj* (jsget e "headers"))))))
        (util/server-error (jsget e "code") message {:cause e})))
    (string? e) (util/server-error nil e)
    (not e) (util/server-error "REJECTED_FALSY" "handler rejected with a falsy value" {:value e})
    :else (util/server-error nil (pr-str e) {:value e})))

(def http-error
  "Re-export of util/http-error: `(throw (http-error 404 \"no such user\"))`
   inside a handler answers that status through the error chain."
  util/http-error)

;; ---------------------------------------------------------------------------
;; Cookies (inventory 4.2, uncertainty 10.3)
;; ---------------------------------------------------------------------------

(defn- try-decode
  "decodeURIComponent that keeps the raw string on a malformed escape."
  [s]
  (try (js/decodeURIComponent s) (catch :default _ s)))

(defn parse-cookies
  "`Cookie` header string -> {name-keyword value-string}: pairs split on
   `;`, the first `=` separates name and value, surrounding double quotes
   are stripped, values are URI-decoded (a malformed escape keeps the raw
   value), and the FIRST occurrence of a name wins — exactly what the
   `cookie` package does. Names become keywords as spelled. nil -> {}."
  [header]
  (if (str/blank? header)
    {}
    (reduce (fn [m pair]
              (let [i (str/index-of pair "=")]
                (if (nil? i)
                  m
                  (let [k (str/trim (subs pair 0 i))
                        v (str/trim (subs pair (inc i)))
                        v (if (and (>= (count v) 2) (str/starts-with? v "\"") (str/ends-with? v "\""))
                            (subs v 1 (dec (count v)))
                            v)]
                    (if (or (str/blank? k) (contains? m (keyword k)))
                      m
                      (assoc m (keyword k) (try-decode v)))))))
            {}
            (str/split header #";"))))

(defn cookie-sign
  "`value.<base64 HMAC-SHA256(secret, value) without trailing =>` —
   byte-compatible with `cookie-signature.sign` (verified by the dev-only
   test). Express prefixes the result with `s:` when it sets a signed
   cookie; `cookie-signed` does the same."
  [value secret]
  (let [mac (-> (js-invoke node-crypto "createHmac" "sha256" secret)
                (js-invoke "update" (str value))
                (js-invoke "digest" "base64")
                (str/replace #"=+$" ""))]
    (str value "." mac)))

(defn cookie-unsign
  "Inverse of cookie-sign: the original value when the signature verifies
   (constant-time comparison), false otherwise (the cookie-parser
   convention for a tampered cookie)."
  [signed secret]
  (let [i (str/last-index-of signed ".")]
    (if (nil? i)
      false
      (let [value (subs signed 0 i)
            expected (js-invoke js/Buffer "from" (cookie-sign value secret))
            actual (js-invoke js/Buffer "from" signed)]
        (if (and (= (jsget expected "length") (jsget actual "length"))
                 (js-invoke node-crypto "timingSafeEqual" expected actual))
          value
          false)))))

(defn- decode-json-cookie
  "`j:{...}` -> parsed CLJS value (cookie-parser's JSONCookie); other
   strings untouched."
  [v]
  (if (and (string? v) (str/starts-with? v "j:"))
    (try (->clj* (js-invoke js/JSON "parse" (subs v 2))) (catch :default _ v))
    v))

(defn cookies-and-signed
  "Splits parsed cookies into [:cookies :signed-cookies] the way
   cookie-parser does: with a secret, every `s:`-prefixed value moves to
   :signed-cookies as its verified value (false when tampered); `j:`
   JSON values are decoded on both sides. Without a secret nothing moves."
  [cookies secret]
  (if (nil? secret)
    [(into {} (map (fn [[k v]] [k (decode-json-cookie v)])) cookies) {}]
    (reduce (fn [[plain signed] [k v]]
              (if (and (string? v) (str/starts-with? v "s:"))
                [plain (assoc signed k (decode-json-cookie (cookie-unsign (subs v 2) secret)))]
                [(assoc plain k (decode-json-cookie v)) signed]))
            [{} {}]
            cookies)))

(def cookie-option-keys
  "res.cookie options (inventory 5.2): :max-age (ms), :expires (js/Date,
   epoch ms or date string), :http-only?, :path (default \"/\"), :domain,
   :secure?, :same-site (true / :lax / :strict / :none), :priority (:low /
   :medium / :high), :partitioned?, :encode (fn), :signed?."
  #{:max-age :expires :http-only :path :domain :secure :same-site :priority
    :partitioned :encode :signed})

(defn- cookie-options->js
  [opts]
  (let [opts (util/check-opts! :cookie cookie-option-keys (util/normalize-opts (dissoc opts :name :value)))
        opts (cond-> (dissoc opts :signed)
               (contains? opts :same-site) (update :same-site util/enum-value)
               (contains? opts :priority) (update :priority util/enum-value)
               (or (number? (:expires opts)) (string? (:expires opts))) (update :expires #(js/Date. %)))]
    (clj->js (util/->camel-keys opts))))

(defn- cookie-value
  "Express's res.cookie value rule (object -> `j:` JSON) plus the wrapper's
   own signing (so `req.secret` is never needed)."
  [value secret {:keys [signed?]}]
  (let [v (if (or (map? value) (sequential? value))
            (str "j:" (js-invoke js/JSON "stringify" (->js* value)))
            (str value))]
    (if signed?
      (do (when (nil? secret)
            (throw (util/argument-error "signed cookie requires the app option :cookie-secret" {:arg :cookie-secret})))
          (str "s:" (cookie-sign v secret)))
      v)))

;; ---------------------------------------------------------------------------
;; Request -> map (inventory 5.1, 11.3)
;; ---------------------------------------------------------------------------

(defn- vec-or-empty
  "JS array -> vector; anything else -> []."
  [v]
  (if (array? v) (vec (array-seq v)) []))

(defn- header-map
  "req.headers -> kebab header map."
  [req]
  (util/->header-map (->clj* (jsget req "headers"))))

(defn- base-map
  "The per-request part of the map (independent of the matched layer),
   computed once and cached on the raw request."
  [app req]
  (or (jsget req base-key)
      (let [headers (header-map req)
            secret (get-in app [:opts :cookie-secret])
            [cookies signed] (cookies-and-signed (parse-cookies (:cookie headers)) secret)
            cl (:content-length headers)
            m {:method (keyword (str/lower-case (str (jsget req "method"))))
               :original-url (jsget req "originalUrl")
               :headers headers
               :cookies cookies
               :signed-cookies signed
               :ip (jsget req "ip")
               :ips (vec-or-empty (jsget req "ips"))
               :protocol (keyword (jsget req "protocol"))
               :secure? (boolean (jsget req "secure"))
               :host (jsget req "host")
               :hostname (jsget req "hostname")
               :subdomains (vec-or-empty (jsget req "subdomains"))
               :xhr? (boolean (jsget req "xhr"))
               :http-version (jsget req "httpVersion")
               :remote-address (jsget req "socket" "remoteAddress")
               :remote-port (jsget req "socket" "remotePort")
               :accepts (vec-or-empty (js-invoke req "accepts"))
               :accepts-encodings (vec-or-empty (js-invoke req "acceptsEncodings"))
               :accepts-charsets (vec-or-empty (js-invoke req "acceptsCharsets"))
               :accepts-languages (vec-or-empty (js-invoke req "acceptsLanguages"))
               :content-type (:content-type headers)
               :content-length (when (string? cl) (let [n (js/parseInt cl 10)] (when-not (js/isNaN n) n)))
               :app-env (:env app)
               :transport :http}]
        (jsset! req base-key m)
        m)))

(defn req->map
  "Builds the kebab request map (inventory 11.3) for the current layer:
   the cached per-request base (headers, cookies, ip, negotiation, ...)
   plus the layer-dependent fields :url :base-url :path :params :query
   :body :route, merged with the context accumulated by `route/continue`
   values. Metadata `:express-cljs/raw {:req :res}` carries the Node
   objects for `with-raw`; it never appears in the printed map."
  [app req res]
  (let [base (base-map app req)
        body (jsget req "body")
        ctx (jsget req ctx-key)]
    (-> base
        (assoc :url (jsget req "url")
               :base-url (or (jsget req "baseUrl") "")
               :path (jsget req "path")
               :params (->clj* (jsget req "params"))
               :query (->clj* (jsget req "query"))
               :body (if (some? body) (->clj* body) nil)
               :route (when-let [rp (jsget req "route" "path")] {:path rp}))
        (cond-> (map? ctx) (merge ctx))
        (with-meta {:express-cljs/raw {:req req :res res}}))))

(defn with-raw
  "Escape hatch: calls `(f raw-req raw-res)` with the Node / Express
   objects behind a request map (for a caller-mounted multer etc.,
   inventory 4.2). Returns f's result. Throws an argument-error when the
   map did not come from `req->map`."
  [req f]
  (let [{:keys [req res]} (:express-cljs/raw (meta req))]
    (when (nil? req)
      (throw (util/argument-error "with-raw: not a request map produced by express-cljs" {:arg :req})))
    (f req res)))

(defn- raw-req
  "The Node request behind a request map (argument-error otherwise)."
  [req]
  (or (get-in (meta req) [:express-cljs/raw :req])
      (throw (util/argument-error "not a request map produced by express-cljs" {:arg :req}))))

(defn accepts?
  "Content negotiation over the Accept header (req.accepts): given one or
   more types / extensions (\"json\", \"text/html\", :json) returns the
   best match as a string, or nil when none is acceptable."
  [req & types]
  (let [r (apply js-invoke (raw-req req) "accepts" (map #(if (keyword? %) (name %) %) types))]
    (when (string? r) r)))

(defn is?
  "req.is: the matched type string when the request body's Content-Type
   matches `type` (\"json\", \"application/*\"), false when it does not,
   and nil when the request has no body at all (pitfall 9.9)."
  [req type]
  (let [r (js-invoke (raw-req req) "is" (if (keyword? type) (name type) type))]
    (cond (nil? r) nil (false? r) false :else r)))

(defn range
  "req.range: parses the Range header against `size` bytes. Returns
   {:type \"bytes\" :ranges [{:start 0 :end 99} ..]}, :unsatisfiable,
   :malformed, or nil when there is no Range header. Options: :combine?
   (merge overlapping ranges)."
  ([req size] (range req size {}))
  ([req size opts]
   (let [r (js-invoke (raw-req req) "range" size (clj->js (util/->camel-keys (util/normalize-opts opts))))]
     (cond
       (nil? r) nil
       (= -1 r) :unsatisfiable
       (= -2 r) :malformed
       :else {:type (jsget r "type")
              :ranges (mapv (fn [x] {:start (jsget x "start") :end (jsget x "end")}) (array-seq r))}))))

(defn- http-date->ms
  [s]
  (let [t (js/Date.parse s)]
    (when (and (number? t) (not (js/isNaN t))) t)))

(defn- parse-token-list
  [s]
  (into [] (comp (map str/trim) (remove str/blank?)) (str/split s #",")))

(defn fresh?
  "The `fresh` algorithm behind req.fresh (inventory 5.1, uncertainty 10.6)
   evaluated against the RESPONSE HEADERS you intend to send (`{:etag ..
   :last-modified ..}`, kebab) rather than headers already set on `res`:
   true when the client's copy is still valid and a 304 is appropriate.
   If-None-Match (weak comparison, `*`) wins over If-Modified-Since;
   `Cache-Control: no-cache` or an unconditional request is never fresh.
   Express also requires a 2xx/304 status; the caller knows the status.
   Takes the request map (or its :headers map)."
  [req res-headers]
  (let [rh (if (contains? req :headers) (:headers req) req)
        modified-since (:if-modified-since rh)
        none-match (:if-none-match rh)
        cache-control (:cache-control rh)]
    (cond
      (and (str/blank? modified-since) (str/blank? none-match)) false
      (and (string? cache-control) (re-find #"(?:^|,)\s*?no-cache\s*?(?:,|$)" cache-control)) false
      (not (str/blank? none-match))
      (if (= "*" none-match)
        true
        (let [etag (:etag res-headers)]
          (and (string? etag)
               (boolean (some (fn [m] (or (= m etag) (= (str "W/" m) etag) (= m (str "W/" etag))))
                              (parse-token-list none-match))))))
      :else
      (let [lm (:last-modified res-headers)
            lm-ms (and (string? lm) (http-date->ms lm))
            ims-ms (http-date->ms modified-since)]
        (boolean (and lm-ms ims-ms (<= lm-ms ims-ms)))))))

(defn stale?
  "(not (fresh? req res-headers))."
  [req res-headers]
  (not (fresh? req res-headers)))

;; ---------------------------------------------------------------------------
;; Response map -> res (inventory 5.2, 11.4)
;; ---------------------------------------------------------------------------

(defn- set-headers!
  [res headers]
  (doseq [[k v] (util/->wire-headers headers)]
    (js-invoke res "set" k (if (vector? v) (to-array v) v))))

(defn- headers-sent? [res] (boolean (jsget res "headersSent")))

(declare respond!)

(defn- pipe-body!
  "Pipes a Readable into res; a stream error before headers are sent goes
   to the error chain, afterwards the connection is destroyed."
  [body res next]
  (when (nil? (js-invoke res "get" "content-type"))
    (js-invoke res "type" "application/octet-stream"))
  (js-invoke body "once" "error"
             (fn [e]
               (if (headers-sent? res)
                 (js-invoke res "destroy" e)
                 (next (->error e)))))
  (js-invoke body "pipe" res))

(defn- file-callback
  [res next]
  (fn [err]
    (when (some? err)
      (if (headers-sent? res)
        (js-invoke res "destroy" err)
        (next (->error err))))))

(defn- send-body!
  [{:keys [body json]} req res next]
  (cond
    (nil? body) (js-invoke res "end")
    (readable? body) (pipe-body! body res next)
    (buffer? body) (js-invoke res "send" body)
    (or json (map? body) (sequential? body) (set? body)) (js-invoke res "json" (->js* body))
    (string? body) (js-invoke res "send" body)
    (keyword? body) (js-invoke res "send" (name body))
    :else (js-invoke res "send" body)))

(defn respond!
  "Executes a response map against `res` exactly once (inventory 5.2):
   validates it (`util/check-response!`), then applies :status (default
   200; 204 for `:end true`), :type, :headers, :vary, :links, :attachment,
   :cookies (with signing), :clear-cookies, and finally the single body
   operation — :body (string / map|vector -> JSON / Buffer / Readable /
   nil), :redirect (\"/x\" or {:url :status}, default 302), :send-file
   ({:path :root ..}), :download ({:path :filename :root ..}), :format
   ({:json (fn [req]) :html .. :default ..}), or :end. Errors raised by
   upstream (sendFile 403/404, format 406) go to `next` as ex-infos.
   Returns nil. The app's cookie secret is read from `app`."
  [app response req res next]
  (let [r (util/check-response! response)
        secret (get-in app [:opts :cookie-secret])]
    (when (headers-sent? res)
      (throw (util/server-error "HEADERS_SENT" "response already sent" {:response r})))
    (js-invoke res "status" (or (:status r) (if (:end r) 204 200)))
    (when-let [t (:type r)] (js-invoke res "type" (if (keyword? t) (name t) (str t))))
    (set-headers! res (:headers r))
    (doseq [v (:vary r)] (js-invoke res "vary" (if (keyword? v) (name v) (str v))))
    (when-let [links (:links r)] (js-invoke res "links" (->js* links)))
    (when-let [a (:attachment r)] (js-invoke res "attachment" (if (string? a) a js/undefined)))
    (doseq [c (:cookies r)]
      (js-invoke res "cookie" (:name c) (cookie-value (:value c) secret {:signed? (or (:signed? c) (:signed c))})
                 (cookie-options->js (dissoc c :signed? :signed))))
    (doseq [c (:clear-cookies r)]
      (js-invoke res "clearCookie" (:name c) (cookie-options->js (dissoc c :signed? :signed))))
    (cond
      (contains? r :redirect)
      (let [x (:redirect r)
            [url status] (if (map? x) [(:url x) (or (:status x) 302)] [x 302])]
        (js-invoke res "redirect" status url))

      (contains? r :send-file)
      (let [{:keys [path] :as o} (:send-file r)
            opts (clj->js (util/->camel-keys (util/normalize-opts (dissoc o :path))))]
        (js-invoke res "sendFile" path opts (file-callback res next)))

      (contains? r :download)
      (let [{:keys [path filename] :as o} (:download r)
            opts (clj->js (util/->camel-keys (util/normalize-opts (dissoc o :path :filename))))]
        (js-invoke res "download" path (or filename path) opts (file-callback res next)))

      (contains? r :format)
      (let [req-map (req->map app req res)
            table (reduce (fn [o [k f]]
                            (jsset! o (name k)
                                    (fn []
                                      (-> (p/do (f req-map))
                                          (p/then (fn [resp] (respond! app (or resp {:end true}) req res next)))
                                          (p/catch (fn [e] (next (->error e))))))))
                          #js {}
                          (:format r))]
        (js-invoke res "format" table))

      (:end r) (js-invoke res "end")

      :else (send-body! r req res next))
    nil))

;; ---------------------------------------------------------------------------
;; Handler adapters (inventory 11.2, 4.3, 8.6)
;; ---------------------------------------------------------------------------

(defn- apply-control!
  "A route/control value -> the matching next() call."
  [{:keys [express-cljs/next assoc headers]} req res next-fn]
  (when (seq assoc)
    (jsset! req ctx-key (merge (jsget req ctx-key) assoc)))
  (when (seq headers) (set-headers! res headers))
  (case next
    :route (next-fn "route")
    :router (next-fn "router")
    (next-fn)))

(defn- dispatch!
  "Handler result -> next() / next('route') / response / nothing."
  [app result req res next]
  (cond
    (nil? result) (next)
    (route/control? result) (apply-control! result req res next)
    :else (respond! app result req res next)))

(defn- run-handler!
  "Runs `(f req-map)` (sync or promise) and dispatches; every failure ends
   as `next(ex-info)`. Returns nil so Express's own promise forwarding
   (inventory 1.4) never sees a value."
  [app f req res next]
  (-> (p/do (f (req->map app req res)))
      (p/then (fn [result] (dispatch! app result req res next)))
      (p/catch (fn [e] (next (->error e)))))
  nil)

(defn- handler-adapter
  "CLJS handler -> Express (req, res, next) middleware (arity 3)."
  [app f]
  (fn [req res next] (run-handler! app f req res next)))

(defn- error-adapter
  "CLJS error handler (fn [err req]) -> Express (err, req, res, next)
   middleware (arity 4 — Express detects error handlers by fn.length)."
  [app f]
  (fn [err req res next]
    (let [e (->error err)]
      (-> (p/do (f e (req->map app req res)))
          (p/then (fn [result]
                    (cond
                      (nil? result) (next e)
                      (route/control? result) (apply-control! result req res next)
                      :else (respond! app result req res next))))
          (p/catch (fn [e2] (next (->error e2)))))
      nil)))

(defn- param-adapter
  "CLJS param loader (fn [req value]) -> Express (req, res, next, value, name)."
  [app f]
  (fn [req res next value _name]
    (-> (p/do (f (req->map app req res) value))
        (p/then (fn [result] (dispatch! app result req res next)))
        (p/catch (fn [e] (next (->error e)))))
    nil))

(defn- emit!
  "Puts an event on the app's events channel (sliding buffer: never
   blocks, oldest dropped)."
  [app ev]
  (when-let [ch (:events app)] (async/put! ch ev)))

(defn- final-error-handler
  "The wrapper's last arity-4 middleware: renders util/error-response,
   emits {:event :error} and calls the app's :on-error."
  [app]
  (fn [err req res _next]
    (let [e (->error err)
          req-map (req->map app req res)
          response (util/error-response e req-map {:env (:env app)
                                                   :expose-errors? (get-in app [:opts :expose-errors])})
          on-error (get-in app [:opts :on-error])]
      (emit! app {:event :error :error e :status (:status response)
                  :method (:method req-map) :path (:original-url req-map)})
      (when on-error (try (on-error e req-map) (catch :default _ nil)))
      (when (and (nil? on-error) (>= (:status response) 500) (not= :test (:env app)))
        (js-invoke js/console "error" (str "express-cljs: " (ex-message e)) (or (:cause (ex-data e)) "")))
      (if (headers-sent? res)
        (js-invoke res "destroy")
        (respond! app response req res (fn [_] (js-invoke res "end")))))))

(defn not-found
  "The wrapper's final 404 middleware as a handler: throws an http-error
   404 {:reason :not-found :path ..} for every request except OPTIONS,
   which continues so the router's automatic `Allow` answer (inventory
   3.3) can fire."
  [req]
  (if (= :options (:method req))
    nil
    (throw (util/http-error 404 "Not Found" {:reason :not-found :path (:original-url req)}))))

;; ---------------------------------------------------------------------------
;; Built-in middleware instantiation (inventory 4.1)
;; ---------------------------------------------------------------------------

(defn- static-middleware
  [root opts]
  (let [opts (cond-> opts
               (contains? opts :dotfiles) (update :dotfiles util/enum-value)
               (contains? opts :index) (update :index (fn [i] (if (sequential? i) (to-array i) i)))
               (contains? opts :extensions) (update :extensions to-array)
               (fn? (:set-headers opts))
               (update :set-headers (fn [f]
                                      (fn [res path stat]
                                        (set-headers! res (f path {:size (jsget stat "size")
                                                                   :mtime-ms (jsget stat "mtimeMs")}))))))]
    (js-invoke express-module "static" root (clj->js (util/->camel-keys opts)))))

(defn- builtin-middleware
  [{:keys [express-cljs/builtin opts root]}]
  (case builtin
    :static (static-middleware root opts)
    (:json :urlencoded :text :raw)
    (let [opts (cond-> opts (sequential? (:type opts)) (update :type to-array))]
      (js-invoke express-module (name builtin) (clj->js (util/->camel-keys opts))))
    (throw (util/argument-error (str "unknown built-in middleware " (pr-str builtin)) {:value builtin}))))

(defn- ->express-fn
  "Any handler position (fn / descriptor) -> the Express function."
  [app h]
  (cond
    (fn? h) (handler-adapter app h)
    (contains? h :express-cljs/builtin) (builtin-middleware h)
    (contains? h :express-cljs/upstream) (:express-cljs/upstream h)
    (contains? h :express-cljs/error-handler) (error-adapter app (:express-cljs/error-handler h))
    :else (throw (util/argument-error (str "not a handler: " (pr-str h)) {:value h}))))

;; ---------------------------------------------------------------------------
;; Plan registration (route data -> Express calls)
;; ---------------------------------------------------------------------------

(defn- registration-error
  [e op path]
  (if (some? (ex-data e))
    e
    (util/argument-error (str "route registration failed for " (name op) " " (pr-str path) ": " (jsget e "message"))
                         {:op op :path path :cause e})))

(defn- ->js-path
  "A vector of paths -> JS array; strings / RegExps untouched."
  [path]
  (if (vector? path) (to-array path) path))

(defn- method-fn-name
  [method]
  (when-not (or (= :all method) (contains? methods method))
    (throw (util/argument-error (str "method " (pr-str method) " is not supported by this Node/Express")
                                {:arg :method :value method :allowed methods})))
  (case method
    :all "all"
    :m-search "m-search"
    (name method)))

(defn- resource-fallback
  [method-map]
  (let [allow (->> (keys method-map)
                   (remove #{:all})
                   (map #(str/upper-case (name %)))
                   (#(if (some #{"GET"} %) (concat % ["HEAD"]) %))
                   distinct
                   (str/join ", "))]
    (fn [req]
      (throw (util/http-error 405 "Method Not Allowed" {:reason :method-not-allowed
                                                       :headers {:allow allow}
                                                       :method (:method req)})))))

(defn- register-op!
  "Executes one plan op against an Express app or Router (`target`)."
  [app target {:keys [op method path handlers] :as o}]
  (try
    (case op
      :use (apply js-invoke target "use" (->js-path path) (map #(->express-fn app %) handlers))
      :route (apply js-invoke target (method-fn-name method) (->js-path path) (map #(->express-fn app %) handlers))
      :param (js-invoke target "param" (:name o) (param-adapter app (:handler o)))
      :router (let [r (js-invoke express-module "Router" (clj->js (util/->camel-keys (:opts o))))]
                (doseq [sub (:plan o)] (register-op! app r sub))
                (js-invoke target "use" (->js-path path) r))
      :resource (let [r (js-invoke target "route" (->js-path path))
                      mm (:methods o)]
                  (doseq [[m hs] mm :when (not= :all m)]
                    (apply js-invoke r (method-fn-name m) (map #(->express-fn app %) hs)))
                  (if-let [hs (:all mm)]
                    (apply js-invoke r "all" (map #(->express-fn app %) hs))
                    (js-invoke r "all" (handler-adapter app (resource-fallback mm)))))
      (throw (util/argument-error (str "unknown plan op " (pr-str op)) {:value o})))
    (catch :default e
      (throw (registration-error e op path)))))

;; ---------------------------------------------------------------------------
;; App
;; ---------------------------------------------------------------------------

(defn setting
  "Reads an Express app setting by kebab keyword (`:trust-proxy`, `:env`,
   `:x-powered-by`, inventory 3.9); nil when unset."
  [app k]
  (let [v (js-invoke (:express app) "get" (util/kebab->setting k))]
    (if (nil? v) nil (->clj* v))))

(defn set-setting!
  "Sets an Express app setting by kebab keyword; keyword values render as
   strings (:extended -> \"extended\"), :trust-proxy goes through
   util/trust-proxy-setting. Routing settings (:case-sensitive-routing,
   :strict-routing) only take effect before the first route is registered.
   Returns the app."
  [app k v]
  (let [v (cond
            (= :trust-proxy k) (util/trust-proxy-setting v)
            (keyword? v) (name v)
            :else v)]
    (js-invoke (:express app) "set" (util/kebab->setting k) v)
    app))

(defn- parser-opts
  "App-level body parser option: true -> {}, a map -> itself, false/nil -> nil."
  [v]
  (cond (true? v) {} (map? v) v :else nil))

(defn app
  "Builds an Express 5 application from options and any number of route
   sources (tables, `route/routes` bundles, entries — see
   express-cljs.route), registered in argument order. Options (kebab,
   inventory 3.9 / 11.2):

     :env                    :production | :development | :test (Express
                             'env'; :test silences the 5xx console log)
     :trust-proxy            false (default) | true | hops | :loopback |
                             \"10.0.0.0/8\" | [..] | (fn [ip i])
     :x-powered-by?          default false (Express's default is true)
     :case-sensitive-routing? :strict-routing?   router flags
     :query-parser           :simple (default) | :extended | false | fn
     :etag                   :weak (default) | :strong | false
     :json-escape? :json-replacer :json-spaces :subdomain-offset
     :cookie-secret          enables :signed-cookies / {:signed? true}
     :expose-errors?         show messages of non-http errors (default:
                             true unless :env is :production)
     :on-error               (fn [err req-map]) called for every rendered
                             error (replaces the default console log)
     :error-handler          (fn [err req-map] -> response | nil): an
                             app-wide error handler run before the
                             wrapper's renderer
     :json :urlencoded :text :raw   true or an option map: mounts that
                             body parser globally before the routes
     :json-limit             shorthand for {:json {:limit ..}}

   The wrapper's 404 and error renderer are appended by `listen!`
   (`finalize!`), so `mount-upstream!` may still add middleware before
   the server starts. Returns the app value."
  [opts & sources]
  (let [opts (util/check-opts! :app util/app-option-keys (util/normalize-opts opts))
        opts (cond-> opts
               (contains? opts :json-limit) (-> (assoc :json (assoc (or (parser-opts (:json opts)) {}) :limit (:json-limit opts)))
                                                (dissoc :json-limit)))
        ex (express-module)
        env (let [e (:env opts)] (cond (keyword? e) e (string? e) (keyword e) :else (keyword (js-invoke ex "get" "env"))))
        plan (apply route/compile sources)
        app {:express ex
             :opts (assoc opts :env env)
             :env env
             :plan plan
             :events (async/chan (async/sliding-buffer 64))
             :state (atom {:finalized? false})}]
    (doseq [[k v] (util/app-settings opts)]
      (js-invoke ex "set" k v))
    (when-let [f (:error-handler opts)]
      (when-not (fn? f) (throw (util/argument-error ":error-handler must be a function" {:arg :error-handler}))))
    (when-let [f (:on-error opts)]
      (when-not (fn? f) (throw (util/argument-error ":on-error must be a function" {:arg :on-error}))))
    (doseq [k [:json :urlencoded :text :raw]
            :let [po (parser-opts (get opts k))]
            :when po]
      (js-invoke ex "use" (builtin-middleware {:express-cljs/builtin k :opts po})))
    (doseq [op plan] (register-op! app ex op))
    app))

(defn finalize!
  "Appends the wrapper's final layers — the app's :error-handler (if any),
   the 404 handler and the error renderer — exactly once. Called by
   `listen!`; call it yourself only when you drive the app through
   `handler`. Returns the app."
  [app]
  (when-not (:finalized? @(:state app))
    (swap! (:state app) assoc :finalized? true)
    (let [ex (:express app)]
      (when-let [f (get-in app [:opts :error-handler])]
        (js-invoke ex "use" (error-adapter app f)))
      (js-invoke ex "use" (handler-adapter app not-found))
      (js-invoke ex "use" (final-error-handler app))))
  app)

(defn mount-upstream!
  "Appends a caller-supplied Express middleware function `(req, res, next)`
   (inventory 4.2: cors / helmet / multer instances the caller requires in
   its own code) at `path` (default \"/\") AFTER the routes given to `app`
   — Express runs layers in registration order, so use this for
   middleware that should see requests no route answered. For middleware
   that must run before the routes (cors, helmet) put
   `(route/use (route/upstream f))` first in the route data instead. Must
   be called before `listen!` (argument-error afterwards: the final 404 /
   error layers are already in place). Returns the app."
  ([app f] (mount-upstream! app "/" f))
  ([app path f]
   (when (:finalized? @(:state app))
     (throw (util/argument-error "mount-upstream!: the app is already finalized (listen! was called)" {:path path})))
   (when-not (fn? f)
     (throw (util/argument-error "mount-upstream!: expected a middleware function" {:arg :f :value f})))
   (js-invoke (:express app) "use" (route/check-path path) f)
   app))

(defn handler
  "The Express application as a Node request listener `(req, res)`, after
   `finalize!` — for embedding in a caller-owned http server. Prefer
   `listen!`."
  [app]
  (:express (finalize! app)))

;; ---------------------------------------------------------------------------
;; Server lifecycle (inventory 5.3)
;; ---------------------------------------------------------------------------

(def listen-option-keys
  "listen! options: :port (0 = ephemeral), :host (default: all
   interfaces, Node's default), :backlog, :request-timeout-ms (Node
   default 300000), :keep-alive-timeout-ms (5000), :headers-timeout-ms
   (60000)."
  #{:port :host :backlog :request-timeout-ms :keep-alive-timeout-ms :headers-timeout-ms})

(defn address
  "{:address \"127.0.0.1\" :family \"IPv4\" :port 43210} of a listening
   server handle; nil once closed."
  [server]
  (some-> (js-invoke (:server server) "address") ->clj*))

(defn listening?
  "True while the server accepts connections."
  [server]
  (boolean (jsget (:server server) "listening")))

(defn connections!
  "Promise of the number of open connections (server.getConnections)."
  [server]
  (p/create (fn [resolve reject]
              (js-invoke (:server server) "getConnections"
                         (fn [err n] (if err (reject (->error err)) (resolve n)))))))

(defn events
  "The app's core.async channel of lifecycle events (inventory 11.5):
   {:event :listening :address ..}, {:event :close}, {:event :error
   :error e :status .. :method .. :path ..} for every rendered error,
   {:event :server-error :error e} for http.Server 'error' / 'clientError'
   events. Sliding buffer of 64: consuming is optional."
  [server-or-app]
  (or (:events server-or-app) (get-in server-or-app [:app :events])))

(defn add-close-hook!
  "Registers `(fn [{:keys [timeout-ms]}] -> promise | any)` to run at the
   start of `close!` (the seam the websocket layer uses to close its
   clients before the HTTP server stops, inventory 5.3). Hooks run
   concurrently; a rejection is emitted as {:event :server-error} and
   does not abort the shutdown. Returns the server handle."
  [server f]
  (swap! (:close-hooks server) conj f)
  server)

(defn listen!
  "Starts an http.Server for `app` (created here, not by app.listen, so the
   wrapper owns the 'upgrade' event and the timeouts) and resolves with the
   server handle once listening. Rejects with a server-error {:code
   :eaddrinuse | :eacces | ..} when the port cannot be bound. Options: see
   listen-option-keys."
  [app opts]
  (let [{:keys [port host backlog request-timeout-ms keep-alive-timeout-ms headers-timeout-ms] :as opts}
        (util/check-opts! :listen listen-option-keys (util/normalize-opts opts))
        port (or port 0)
        ex (handler app)
        raw (js-invoke node-http "createServer" ex)
        server {:server raw :app app :events (:events app) :close-hooks (atom [])}]
    (when request-timeout-ms (jsset! raw "requestTimeout" request-timeout-ms))
    (when keep-alive-timeout-ms (jsset! raw "keepAliveTimeout" keep-alive-timeout-ms))
    (when headers-timeout-ms (jsset! raw "headersTimeout" headers-timeout-ms))
    (p/create
     (fn [resolve reject]
       (let [settled (atom false)
             on-error (fn [err]
                        (let [e (->error err)]
                          (if (compare-and-set! settled false true)
                            (reject (util/server-error (jsget err "code") (jsget err "message")
                                                       {:op :listen :port port :host host :cause err}))
                            (emit! app {:event :server-error :error e}))))]
         (js-invoke raw "on" "error" on-error)
         (js-invoke raw "on" "clientError"
                    (fn [err socket]
                      (emit! app {:event :server-error :error (->error err)})
                      (when (and (some? socket) (jsget socket "writable"))
                        (js-invoke socket "end" "HTTP/1.1 400 Bad Request\r\n\r\n"))))
         (js-invoke raw "on" "close" (fn [] (emit! app {:event :close})))
         (js-invoke raw "listen" port host backlog
                    (fn []
                      (when (compare-and-set! settled false true)
                        (let [s (assoc server :address (address server))]
                          (emit! app {:event :listening :address (:address s)})
                          (resolve s))))))))))

(def close-option-keys
  "close! options: :timeout-ms (default 5000: after it, in-flight HTTP
   requests are cut with closeAllConnections), :ws-close-timeout-ms
   (default 2000: passed to the close hooks as :timeout-ms)."
  #{:timeout-ms :ws-close-timeout-ms})

(defn close!
  "Graceful shutdown (inventory 5.3 house choreography): (1) run the close
   hooks (websocket clients) with :ws-close-timeout-ms, (2) server.close —
   stop accepting; Node >= 19 also reaps idle keep-alive connections, and
   closeIdleConnections is called explicitly for older Nodes, (3) after
   :timeout-ms (unref'd timer) closeAllConnections cuts stuck requests,
   (4) resolve {:closed true :forced? bool} when the close callback fires.
   Rejects with a server-error {:code :err-server-not-running} when the
   server is not listening. Afterwards the process holds no handle from
   this server (liveness precedent)."
  ([server] (close! server {}))
  ([server opts]
   (let [{:keys [timeout-ms ws-close-timeout-ms] :or {timeout-ms 5000 ws-close-timeout-ms 2000}}
         (util/check-opts! :close close-option-keys (util/normalize-opts opts))
         raw (:server server)
         app (:app server)]
     (if-not (listening? server)
       (p/rejected (util/server-error "ERR_SERVER_NOT_RUNNING" "server is not listening" {:op :close}))
       (p/let [_ (p/all (map (fn [hook]
                               (-> (p/do (hook {:timeout-ms ws-close-timeout-ms}))
                                   (p/catch (fn [e] (emit! app {:event :server-error :error (->error e)})))))
                             @(:close-hooks server)))]
         (p/create
          (fn [resolve reject]
            (let [forced (atom false)
                  timer (js/setTimeout (fn []
                                         (reset! forced true)
                                         (js-invoke raw "closeAllConnections"))
                                       timeout-ms)]
              (js-invoke timer "unref")
              (js-invoke raw "close"
                         (fn [err]
                           (js/clearTimeout timer)
                           (if (some? err)
                             (reject (->error err))
                             (resolve {:closed true :forced? @forced}))))
              (js-invoke raw "closeIdleConnections")))))))))
