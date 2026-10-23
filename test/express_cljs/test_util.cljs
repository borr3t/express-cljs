(ns express-cljs.test-util
  "Shared fixtures for the express-cljs tests. Not a test namespace itself
   (the name does not end in -test). This is the ONLY test file with
   JavaScript interop (inventory 11.6): the loopback HTTP client (Node's
   global `fetch`), the raw-socket helper, the process resource probe and
   the dev-only `cookie-signature` byte-compatibility oracle live here so
   the test namespaces stay interop-free."
  (:require [cljs.core.async :as async]
            [cljs.test :refer [is] :as ct]
            [clojure.string :as str]
            [goog.object :as gobj]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.util :as util]))

(defn rejection
  "Promise of the rejection reason of `p` (fails the assertion if it resolves)."
  ;; NB: promesa treats a js/Error RETURNED from a catch handler as a
  ;; rejection (IPromiseFactory on js/Error), so the error must be wrapped in
  ;; an already-resolved promise to travel as a value.
  [promise]
  (-> promise
      (p/then (fn [v] (throw (ex-info "expected rejection" {:resolved v}))))
      (p/catch (fn [e] (p/resolved e)))))

(defn thrown
  "The value thrown by (f), nil when it returns normally."
  [f]
  (try (f) nil (catch :default e e)))

(defn env-var
  "process.env lookup."
  [k]
  (gobj/get (gobj/get js/process "env") k))

(defn trace!
  "Prints `msg` (prefixed with the running test's name) when the
   EXPRESS_CLJS_TRACE environment variable is set — for locating a test
   that keeps the process alive."
  [& msg]
  (when (env-var "EXPRESS_CLJS_TRACE")
    (println "  [trace]" (some-> (ct/get-current-env) :testing-vars first meta :name) (apply str msg))))

(defn finish
  "Runs promise `p` to completion inside an (async done ...) block: an
   unexpected rejection fails the test with its message; `done` is always
   called."
  [done p]
  (trace! "start")
  (-> (p/promise p)
      (p/catch (fn [e]
                 (is false (str "unexpected rejection: " (ex-message e) " " (pr-str (ex-data e))))))
      (p/finally (fn [_ _] (trace! "done") (done)))))

(defn sleep
  "Promise resolved after `ms` milliseconds."
  [ms]
  (p/delay ms nil))

;; ---------------------------------------------------------------------------
;; Loopback HTTP client
;; ---------------------------------------------------------------------------

(defn- response->map
  [resp]
  (let [headers (gobj/get resp "headers")
        hm (atom {})]
    (js-invoke headers "forEach" (fn [v k] (swap! hm assoc (util/header-keyword k) v)))
    (p/let [text (js-invoke resp "text")]
      {:status (gobj/get resp "status")
       :headers @hm
       :set-cookies (vec (array-seq (js-invoke headers "getSetCookie")))
       :body text
       :json (when (str/includes? (str (:content-type @hm)) "json")
               (try (js->clj (js-invoke js/JSON "parse" text) :keywordize-keys true)
                    (catch :default _ ::unparseable)))})))

(defn fetch!
  "Loopback request with Node's global fetch. `url` is absolute; `opts`:
   :method (keyword), :headers (kebab map), :body (string or map -> JSON
   with content-type), :redirect (:manual default, so 3xx are observed).
   Resolves {:status :headers (kebab map, folded) :set-cookies [..] :body
   text :json parsed-when-json}. Rejects with the fetch error (socket hang
   up etc.)."
  ([url] (fetch! url {}))
  ([url {:keys [method headers body redirect] :or {method :get redirect :manual}}]
   (let [json-body? (map? body)
         headers (cond-> (or headers {})
                   json-body? (assoc :content-type "application/json"))
         init (cond-> {"method" (str/upper-case (name method))
                       "headers" (clj->js (util/->wire-headers headers))
                       "redirect" (name redirect)}
                (some? body) (assoc "body" (if json-body? (js-invoke js/JSON "stringify" (clj->js body)) body)))]
     (p/then (js/fetch url (clj->js init)) response->map))))

(defn base-url
  "http://127.0.0.1:<port> of a server handle."
  [server]
  (str "http://127.0.0.1:" (:port (core/address server))))

(defn with-server
  "Fixture: listens `app` on an ephemeral loopback port, calls `(f server
   base-url)` (may return a promise), then closes the server — always —
   and asserts the port is released. Resolves with f's value."
  ([app f] (with-server app {} f))
  ([app listen-opts f]
   (p/let [server (core/listen! app (merge {:port 0 :host "127.0.0.1"} listen-opts))]
     (-> (p/do (f server (base-url server)))
         (p/finally (fn [_ _]
                      (p/let [_ (when (core/listening? server) (core/close! server {:timeout-ms 1000}))]
                        (is (not (core/listening? server)) "server closed by fixture"))))))))

(defn active-resources
  "process.getActiveResourcesInfo() as a vector of strings (liveness
   evidence: no TCPServerWrap after close!)."
  []
  (vec (array-seq (js-invoke js/process "getActiveResourcesInfo"))))

(defn layer-arities
  "`fn.length` of every layer function of the finalized app's router
   stack, in registration order — the one number Express reads to tell an
   error handler (4) from a request handler (3 or fewer). Read from the
   compiled functions, so it reports what the optimization level actually
   produced."
  [app]
  (mapv #(gobj/get (gobj/get % "handle") "length")
        (array-seq (gobj/get (gobj/get (core/handler app) "router") "stack"))))

(defn cookie-signature-sign
  "Dev-only oracle: `cookie-signature.sign` from the package Express itself
   depends on (never a runtime dependency of the wrapper)."
  [value secret]
  (js-invoke (js/require "cookie-signature") "sign" value secret))

(defn cookie-signature-unsign
  "Dev-only oracle: `cookie-signature.unsign`."
  [signed secret]
  (js-invoke (js/require "cookie-signature") "unsign" signed secret))

(defn js-error
  "A plain JavaScript Error (to exercise the foreign-error path without
   interop in the test namespaces)."
  [msg]
  (js/Error. msg))

(defn buffer-of
  "A Node Buffer of a UTF-8 string (for raw / binary body tests)."
  [s]
  (js-invoke js/Buffer "from" s "utf8"))

(defn buffer->string
  "UTF-8 string of a Buffer / Uint8Array."
  [b]
  (js-invoke b "toString" "utf8"))

(defn upstream-middleware
  "A hand-written Express middleware `(req, res, next)` that sets a
   response header and continues — stands in for a caller-required cors /
   helmet in the mount-upstream tests."
  [header value]
  (fn [_req res next]
    (js-invoke res "set" header value)
    (next)))

(defn upstream-responder
  "A hand-written Express middleware that answers every request itself
   with `body` (text) — stands in for a caller-required static/proxy
   middleware in the mount-upstream tests."
  [body]
  (fn [_req res _next]
    (js-invoke res "send" body)))

(defn temp-dir
  "A fresh temporary directory populated with {relative-path content}
   (strings). Returns the absolute directory path."
  [files]
  (let [fs (js/require "fs")
        path (js/require "path")
        os (js/require "os")
        dir (js-invoke fs "mkdtempSync" (js-invoke path "join" (js-invoke os "tmpdir") "express-cljs-"))]
    (doseq [[rel content] files]
      (let [full (js-invoke path "join" dir rel)]
        (js-invoke fs "mkdirSync" (js-invoke path "dirname" full) #js {:recursive true})
        (js-invoke fs "writeFileSync" full content)))
    dir))


;; ---------------------------------------------------------------------------
;; Phase 4 additions: websocket test seams (interop stays here)
;; ---------------------------------------------------------------------------

(defn ws-url
  "ws://127.0.0.1:<port><path> of a server handle."
  [server path]
  (str "ws://127.0.0.1:" (:port (core/address server)) path))

(defn js-error-with-code
  "A JS Error carrying a `code` property (to exercise the WS_ERR_* mapping)."
  [msg code]
  (let [e (js/Error. msg)]
    (gobj/set e "code" code)
    e))

(defn pause-socket!
  "Stops a connection's underlying socket from reading (the peer's close
   frame is never seen, so the closing handshake cannot complete) — the
   'unresponsive peer' fixture for the shutdown deadline tests."
  [conn]
  (js-invoke (:ws (:express-cljs/raw (meta conn))) "pause")
  conn)

(defn resume-socket!
  "Undoes pause-socket!. NB: a paused ws socket never emits 'close' (the
   receiver stops draining), so resume before awaiting its close."
  [conn]
  (js-invoke (:ws (:express-cljs/raw (meta conn))) "resume")
  conn)

(defn string-of-bytes
  "An ASCII string of `n` bytes."
  [n]
  (js-invoke (js-invoke js/Buffer "alloc" n 120) "toString" "utf8"))

(defn take-with-timeout
  "Promise of the next value of core.async channel `ch`, ::closed when it
   closes, or ::timeout after `ms` (default 2000) — so a test never hangs
   on a channel that stays silent. Uses alts! so a timed-out take is
   cancelled (a stale taker would steal the next value)."
  ([ch] (take-with-timeout ch 2000))
  ([ch ms]
   (p/create (fn [resolve _]
               (async/go
                 (let [[v port] (async/alts! [ch (async/timeout ms)])]
                   (resolve (cond (not= port ch) ::timeout (nil? v) ::closed :else v))))))))

(defn drain-until-closed
  "Promise of ::closed once `ch` closes (buffered values are discarded),
   or ::timeout when a take waits longer than `ms`."
  ([ch] (drain-until-closed ch 200))
  ([ch ms]
   (p/loop []
     (p/let [v (take-with-timeout ch ms)]
       (if (or (= ::closed v) (= ::timeout v)) v (p/recur))))))

(defn collect
  "Promise of the next `n` values taken from `ch` (each within `ms`)."
  ([ch n] (collect ch n 2000))
  ([ch n ms]
   (p/loop [acc [] i n]
     (if (zero? i)
       acc
       (p/let [v (take-with-timeout ch ms)]
         (p/recur (conj acc v) (dec i)))))))

(defn set-exit-code!
  "process.exitCode = n (the process still exits by itself once the event
   loop drains — the smoke's liveness evidence)."
  [n]
  (gobj/set js/process "exitCode" n))

(defn on-exit!
  "Registers `(fn [code])` on process 'exit'."
  [f]
  (js-invoke js/process "on" "exit" (fn [code] (f code))))

(defn now-ms
  "Date.now()."
  []
  (js/Date.now))
