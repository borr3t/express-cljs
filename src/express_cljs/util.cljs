(ns express-cljs.util
  "Small shared helpers: keyword case translation between kebab-case
   ClojureScript keywords and the three naming dialects of the wrapped
   surface (camelCase option names of express middleware / ws, the
   space-separated app setting names of express, and lower-case HTTP header
   names), option normalisation, and ex-info error construction and
   detection.

   Pure: no I/O and no JavaScript interop. Converting CLJS maps to JS option
   objects, reading fields off req / res / socket objects and translating
   thrown JS errors is interop and therefore lives in the single binding
   namespace (express-cljs.core, Phase 2); this namespace only deals in
   strings, keywords and maps. Later phases add request/response map
   normalisation here (inventory §10)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; kebab-case <-> camelCase (inventory 10.1: express.json/static/urlencoded
;; options, res.cookie options and every ws option are camelCase)
;; ---------------------------------------------------------------------------

(defn kebab->camel
  "\"max-age\" -> \"maxAge\". Keywords render as their name (the namespace is
   dropped); strings pass through untouched (they are assumed to be exact
   upstream names already). Single-word names are unchanged (:limit ->
   \"limit\")."
  [k]
  (cond
    (keyword? k) (let [[head & tail] (str/split (name k) #"-")]
                   (apply str head (map str/capitalize tail)))
    (string? k) k
    :else (str k)))

(defn camel->kebab
  "\"maxAge\" -> :max-age, \"perMessageDeflate\" -> :per-message-deflate.
   Works on strings and keywords, returns a keyword. Namespaced keywords are
   returned unchanged."
  [k]
  (if (and (keyword? k) (namespace k))
    k
    (-> (name k)
        (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
        str/lower-case
        keyword)))

(defn transform-keys
  "Recursively applies `f` to every map key in `x` (maps inside vectors/lists
   included). Keys listed in `passthrough` (a set) keep their subtree
   untouched — user-owned values (a JSON body, a cookie value, `:locals`)
   must never be re-cased."
  ([f x] (transform-keys f #{} x))
  ([f passthrough x]
   (cond
     (map? x) (into (empty x)
                    (map (fn [[k v]]
                           (if (contains? passthrough k)
                             [(f k) v]
                             [(f k) (transform-keys f passthrough v)])))
                    x)
     (sequential? x) (into (empty x) (map #(transform-keys f passthrough %)) x)
     :else x)))

(def option-passthrough-keys
  "Kebab keys whose subtree is handed upstream verbatim: user data and
   opaque callbacks / objects (inventory 10.1)."
  #{:body :locals :headers :set-headers :verify :reviver :replacer :type
    :handle-protocols :verify-client :server :query :params :data})

(defn ->camel-keys
  "Kebab keyword option map -> string-keyed camelCase map, nil values
   dropped, nested option maps translated, subtrees under
   option-passthrough-keys untouched. nil in -> nil out. Pure: the result is
   still a CLJS map; the binding namespace turns it into a JS object."
  [opts]
  (when (some? opts)
    (transform-keys kebab->camel option-passthrough-keys
                    (into {} (remove (comp nil? val)) opts))))

(defn ->kebab-keys
  "Deeply translates camelCase string/keyword keys (as js->clj yields them)
   to kebab keywords; values are never touched."
  [x]
  (transform-keys camel->kebab x))

(defn enum-value
  "Keyword enum value -> upstream string (:lax -> \"lax\", :same-origin ->
   \"sameOrigin\"); non-keywords untouched."
  [v]
  (if (keyword? v) (kebab->camel v) v))

;; ---------------------------------------------------------------------------
;; express app settings (inventory 3.9: `app.set('trust proxy', ..)` names
;; are lower-case words separated by spaces, not camelCase)
;; ---------------------------------------------------------------------------

(defn kebab->setting
  "\":trust-proxy\" -> \"trust proxy\", :x-powered-by -> \"x-powered-by\"
   (the two dashed settings are listed in dashed-settings). Strings pass
   through untouched."
  [k]
  (cond
    (keyword? k) (if (contains? #{:x-powered-by :etag} k)
                   (name k)
                   (str/replace (name k) "-" " "))
    (string? k) k
    :else (str k)))

(defn setting->kebab
  "\"trust proxy\" -> :trust-proxy, \"x-powered-by\" -> :x-powered-by."
  [s]
  (keyword (str/replace (str/lower-case (name s)) #"\s+" "-")))

;; ---------------------------------------------------------------------------
;; HTTP header names (inventory 9.1: header names are case-insensitive on the
;; wire and Node lower-cases every incoming one; the house policy is a
;; lower-case kebab keyword, :content-type, in every map the library returns
;; and accepts)
;; ---------------------------------------------------------------------------

(defn header-name
  "Header keyword or string -> the lower-case wire name (:Content-Type ->
   \"content-type\", \"X-Request-Id\" -> \"x-request-id\")."
  [k]
  (str/lower-case (if (keyword? k) (name k) (str k))))

(defn header-keyword
  "Wire header name -> lower-case kebab keyword (\"Content-Type\" ->
   :content-type). Keywords are lower-cased too."
  [k]
  (keyword (header-name k)))

(defn ->header-map
  "{\"Content-Type\" \"text/plain\"} -> {:content-type \"text/plain\"}. Values
   are never touched (set-cookie stays a vector of strings). nil -> {}."
  [m]
  (into {} (map (fn [[k v]] [(header-keyword k) v])) m))

(defn ->wire-headers
  "{:content-type \"text/plain\" :x-nil nil} -> {\"content-type\"
   \"text/plain\"}: string keys, lower-case, nil values dropped, keyword
   values rendered by name, sequential values kept as vectors (multi-valued
   headers). nil -> {}."
  [m]
  (into {}
        (keep (fn [[k v]]
                (when (some? v)
                  [(header-name k)
                   (cond
                     (keyword? v) (name v)
                     (sequential? v) (mapv #(if (keyword? %) (name %) (str %)) v)
                     :else (str v))])))
        m))

;; ---------------------------------------------------------------------------
;; HTTP status codes (pure data, IANA registry) — reason phrases for the
;; wrapper's own error responses and the RPC transport mapping.
;; ---------------------------------------------------------------------------

(def reason-phrases
  "IANA status code -> reason phrase for the codes the wrapper emits or
   classifies. Not exhaustive; unknown codes print as the bare number."
  {100 "Continue" 101 "Switching Protocols"
   200 "OK" 201 "Created" 202 "Accepted" 204 "No Content" 206 "Partial Content"
   301 "Moved Permanently" 302 "Found" 303 "See Other" 304 "Not Modified"
   307 "Temporary Redirect" 308 "Permanent Redirect"
   400 "Bad Request" 401 "Unauthorized" 402 "Payment Required" 403 "Forbidden"
   404 "Not Found" 405 "Method Not Allowed" 406 "Not Acceptable"
   408 "Request Timeout" 409 "Conflict" 410 "Gone" 411 "Length Required"
   412 "Precondition Failed" 413 "Payload Too Large" 414 "URI Too Long"
   415 "Unsupported Media Type" 416 "Range Not Satisfiable"
   422 "Unprocessable Entity" 426 "Upgrade Required" 428 "Precondition Required"
   429 "Too Many Requests" 431 "Request Header Fields Too Large"
   500 "Internal Server Error" 501 "Not Implemented" 502 "Bad Gateway"
   503 "Service Unavailable" 504 "Gateway Timeout" 505 "HTTP Version Not Supported"})

(defn reason-phrase
  "\"Not Found\" for 404; the number as a string for unknown codes."
  [status]
  (get reason-phrases status (str status)))

(defn valid-status?
  "True for an integer in 100..599."
  [status]
  (and (integer? status) (<= 100 status 599)))

;; ---------------------------------------------------------------------------
;; Option normalisation (inventory 10.1: `?`-suffixed booleans; upstream
;; silently ignores unknown option keys, so the wrapper whitelists them)
;; ---------------------------------------------------------------------------

(defn strip-question-mark
  "`:case-sensitive?` -> :case-sensitive. Keywords keep their namespace;
   anything that is not a keyword is returned unchanged."
  [k]
  (if (keyword? k)
    (keyword (namespace k) (str/replace (name k) #"\?$" ""))
    k))

(defn normalize-opts
  "Strips the `?` suffix from every top-level keyword key of `opts` and
   drops nil-valued entries. nil -> {}."
  [opts]
  (into {}
        (keep (fn [[k v]] (when (some? v) [(strip-question-mark k) v])))
        opts))

(declare argument-error)

(defn check-opts!
  "Throws an argument-error when `opts` (already normalised) is not a map or
   contains a key outside `allowed` (a set of kebab keywords without `?`).
   Returns `opts`."
  [op allowed opts]
  (when-not (map? opts)
    (throw (argument-error (str (name op) ": options must be a map")
                           {:arg :opts :value opts :op op})))
  (let [unknown (remove allowed (keys opts))]
    (when (seq unknown)
      (throw (argument-error (str (name op) ": unknown option(s) " (pr-str (vec unknown))
                                  "; allowed: " (pr-str (vec (sort allowed))))
                             {:arg :opts :unknown (set unknown) :allowed allowed :op op}))))
  opts)

;; ---------------------------------------------------------------------------
;; Errors (inventory §8)
;; ---------------------------------------------------------------------------

(def error-types
  "Every :type an express-cljs ex-info can carry.
   :express-cljs/http-error   — an HTTP failure the wrapper (or a handler)
                                wants sent to the client: :status, :message,
                                optional :headers / :body / :expose?.
   :express-cljs/server-error — lifecycle / transport failure (listen,
                                close, upstream throw), :code from the JS
                                error when present.
   :express-cljs/ws-error     — websocket failure: :code (close code),
                                :reason, :cause.
   :express-cljs/rpc-error    — RPC method failure: :code (keyword, see
                                rpc-error-codes), :message, :data, :id.
   :express-cljs/argument-error — validation before any upstream call."
  #{:express-cljs/http-error :express-cljs/server-error :express-cljs/ws-error
    :express-cljs/rpc-error :express-cljs/argument-error})

(defn http-error
  "ex-info for an HTTP error response. `status` must be 400..599 (an
   argument-error otherwise); `message` defaults to the reason phrase.
   Data: {:type :express-cljs/http-error :status .. :message .. :expose? ..}
   plus `data` (:headers, :body, :cause). `:expose?` defaults to true for
   4xx and false for 5xx (the http-errors convention, inventory 8.3): only
   exposed messages are sent to the client."
  ([status] (http-error status nil nil))
  ([status message] (http-error status message nil))
  ([status message data]
   (when-not (and (valid-status? status) (>= status 400))
     (throw (argument-error (str "http-error: status must be an integer 400..599, got " (pr-str status))
                            {:arg :status :value status})))
   (let [message (or message (reason-phrase status))]
     (ex-info (str "HTTP " status " " message)
              (merge {:type :express-cljs/http-error
                      :status status
                      :message message
                      :expose? (< status 500)}
                     data)))))

(defn server-error
  "ex-info for a server lifecycle or transport failure (listen EADDRINUSE,
   close on a stopped server, an upstream throw the wrapper could not
   classify). Data: {:type :express-cljs/server-error :code <kebab keyword
   of the JS error code or nil> :message .. :op ..}."
  [code message & [data]]
  (let [code (when (and (string? code) (not (str/blank? code)))
               (keyword (str/lower-case (str/replace code "_" "-"))))]
    (ex-info (str "express-cljs" (when code (str " " (name code))) ": " message)
             (merge {:type :express-cljs/server-error
                     :code code
                     :message message}
                    data))))

(defn ws-error
  "ex-info for a websocket failure. `code` is a close code (1000..4999) or
   nil. Data: {:type :express-cljs/ws-error :code .. :reason .. :message ..}."
  [message & [data]]
  (ex-info (str "express-cljs ws: " message)
           (merge {:type :express-cljs/ws-error :message message} data)))

(def rpc-error-codes
  "RPC error keyword -> the JSON-RPC 2.0 numeric code it maps to on the wire
   (inventory §7.5) and the HTTP status the HTTP transport answers with."
  {:parse-error      {:code -32700 :status 400}
   :invalid-request  {:code -32600 :status 400}
   :method-not-found {:code -32601 :status 404}
   :invalid-params   {:code -32602 :status 400}
   :internal-error   {:code -32603 :status 500}
   :unauthorized     {:code -32001 :status 401}
   :forbidden        {:code -32002 :status 403}
   :timeout          {:code -32003 :status 504}
   :application      {:code -32000 :status 500}})

(defn rpc-error
  "ex-info for an RPC failure. `code` is one of the rpc-error-codes keys
   (anything else is an argument-error). Data: {:type
   :express-cljs/rpc-error :code <keyword> :message .. :data .. :id ..}."
  [code message & [data]]
  (when-not (contains? rpc-error-codes code)
    (throw (argument-error (str "rpc-error: unknown code " (pr-str code))
                           {:arg :code :value code :allowed (set (keys rpc-error-codes))})))
  (ex-info (str "express-cljs rpc " (name code) ": " message)
           (merge {:type :express-cljs/rpc-error :code code :message message} data)))

(defn argument-error
  "ex-info for an invalid argument detected before any upstream call."
  [message & [data]]
  (ex-info (str "express-cljs: " message)
           (merge {:type :express-cljs/argument-error} data)))

(defn error-data
  "Returns the structured data of an express-cljs error, or nil when `e` is
   not one."
  [e]
  (when-let [d (ex-data e)]
    (when (contains? error-types (:type d))
      d)))

(defn error-type
  "One of the express-cljs error :type keywords (see error-types) or nil."
  [e]
  (:type (error-data e)))

(defn error-status
  "The HTTP status to answer with for any thrown value: the :status of an
   http-error, the mapped status of an rpc-error, 400 for an
   argument-error, a numeric :status / :status-code field on any other
   ex-info or JS error data map when in 400..599, else 500 (inventory
   8.2: express 5 itself honours err.status / err.statusCode the same way)."
  [e]
  (let [d (or (ex-data e) {})
        t (error-type e)]
    (cond
      (= :express-cljs/http-error t) (:status d)
      (= :express-cljs/rpc-error t) (get-in rpc-error-codes [(:code d) :status] 500)
      (= :express-cljs/argument-error t) 400
      (and (valid-status? (:status d)) (>= (:status d) 400)) (:status d)
      (and (valid-status? (:status-code d)) (>= (:status-code d) 400)) (:status-code d)
      :else 500)))

(defn rpc-error-code
  "The keyword :code of an rpc-error, nil otherwise."
  [e]
  (when (= :express-cljs/rpc-error (error-type e))
    (:code (error-data e))))
