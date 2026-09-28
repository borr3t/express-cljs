(ns express-cljs.route
  "Routing as data. Pure ClojureScript: this namespace
   never touches Express; it turns route descriptions into a *registration
   plan* (a vector of op maps) that `express-cljs.core/app` executes in
   order. Two equivalent input forms exist and can be mixed freely:

   1. The TABLE form — a map keyed by method / kind:

        {:param   {\"id\" (fn [req value] ...)}          ; pre-loaders, first
         :use     [(mw/json) [\"/api\" auth-mw]]          ; middleware, then
         :routers [{:mount \"/admin\" :routes {...}}]     ; nested routers, then
         :get     {\"/users/me\" me-handler               ; routes, sorted by
                   \"/users/:id\" user-handler            ; path specificity
                   \"/files/*splat\" [mw handler]}        ; vector = chain
         :post    {\"/users\" create}
         :all     {\"/legacy/*rest\" gone}
         :resource {\"/things\" {:get list :post create}} ; 405 for the rest
         :error   [(fn [err req] ...)]}                  ; error handlers, last

      Maps are unordered, so the table form derives Express's registration
      order deterministically: kinds run in the order above, and within the
      route kinds every path is sorted by SPECIFICITY (segment by segment:
      literal before parameter before optional group before wildcard; equal
      shapes by path string; a specific method before :all). `/users/me`
      therefore always precedes `/users/:id` regardless of how the map was
      written. RegExp paths sort last (their shape is opaque) — use the
      ordered form when their position matters.

   2. The ORDERED form — `(routes entry ...)` or a plain vector of entries,
      registered exactly in source order (Express semantics).
      Entries come from the constructors below (`get`, `post`, `use`,
      `router`, `resource`, `static`, `param`, `error`, ...), from the tuple
      shorthand `[:get \"/path\" handler ...]` / `[:use \"/path\" mw ...]`, or
      are tables / nested `routes` values.

   Handlers are `(fn [req-map] -> response-map | nil | promise)`; nil means
   \"not mine, continue\" (Express `next()`); returning `(next-route)` /
   `(next-router)` is Express `next('route')` / `next('router')`; returning
   `(continue {:user u})` continues AND merges keys into the request map
   every later handler sees (the wrapper's replacement for mutating `req`).
   A handler position may also hold a middleware descriptor from
   `express-cljs.middleware` (body parsers, static files, a caller-supplied
   upstream Express middleware) or a vector of handlers (a chain).

   `check-path` lints the three Express 4 spellings that path-to-regexp 8
   rejects so the failure names the route, not an index."
  (:refer-clojure :exclude [get use compile methods])
  (:require [clojure.string :as str]
            [express-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Methods
;; ---------------------------------------------------------------------------

(def methods
  "The lower-case method keywords Express 5 exposes on Node 22 (derived
   from http.METHODS, 35 names including :query). `core/methods`
   is the runtime truth; this static set validates tables before any
   binding call."
  #{:acl :bind :checkout :connect :copy :delete :get :head :link :lock :m-search
    :merge :mkactivity :mkcalendar :mkcol :move :notify :options :patch :post
    :propfind :proppatch :purge :put :query :rebind :report :search :source
    :subscribe :trace :unbind :unlink :unlock :unsubscribe})

(def table-kinds
  "Non-method keys a table may carry, in registration order."
  [:param :use :routers :resource :error])

;; ---------------------------------------------------------------------------
;; Flow-control sentinels (Express's next('route') / next('router'))
;; ---------------------------------------------------------------------------

(defn next-route
  "Return this from a handler to skip the remaining handlers of the current
   route and continue with the next matching route (Express next('route'))."
  []
  {:express-cljs/next :route})

(defn next-router
  "Return this from a handler to leave the current router and continue in
   its parent (Express next('router'))."
  []
  {:express-cljs/next :router})

(defn continue
  "Return this from a handler / param loader to call next() while merging
   `assoc-map` into the request map of every later handler (e.g. `{:user u}`
   after authentication). Optional `headers` are set on the eventual
   response right away (`{:x-request-id id}`)."
  ([] (continue nil nil))
  ([assoc-map] (continue assoc-map nil))
  ([assoc-map headers]
   (cond-> {:express-cljs/next :next}
     (seq assoc-map) (assoc :assoc assoc-map)
     (seq headers) (assoc :headers headers))))

(defn control?
  "True for a value produced by next-route / next-router / continue."
  [v]
  (and (map? v) (contains? v :express-cljs/next)))

;; ---------------------------------------------------------------------------
;; Handler descriptors
;; ---------------------------------------------------------------------------

(defn descriptor?
  "True for a middleware descriptor map: a built-in (`{:express-cljs/builtin
   :json ...}`), a caller-supplied upstream Express middleware
   (`{:express-cljs/upstream f}`) or an error handler
   (`{:express-cljs/error-handler f}`)."
  [v]
  (and (map? v)
       (boolean (some #(contains? v %) [:express-cljs/builtin :express-cljs/upstream :express-cljs/error-handler]))))

(defn handler?
  "True for anything that may sit in a handler position: a function or a
   descriptor."
  [v]
  (or (fn? v) (descriptor? v)))

(defn error-handler
  "Descriptor for an error handler `(fn [err req] -> response-map | nil |
   promise)`: runs only while an error is in the pipeline (Express arity-4
   middleware); nil passes the error on, a response map
   answers, a throw replaces the error. `err` is always an ex-info (foreign
   errors are normalised by the binding)."
  [f]
  (when-not (fn? f)
    (throw (util/argument-error "error-handler: expected a function" {:arg :f :value f})))
  {:express-cljs/error-handler f})

(defn upstream
  "Descriptor for a caller-supplied Express middleware function
   `(req, res, next)` (a `js/require`d cors / helmet / multer instance).
   The wrapper never requires such packages itself."
  [f]
  (when-not (fn? f)
    (throw (util/argument-error "upstream: expected a middleware function" {:arg :f :value f})))
  {:express-cljs/upstream f})

(defn- flatten-handlers
  "Handler args (fns, descriptors, vectors thereof) -> flat vector; throws
   on anything else or when empty."
  [op path handlers]
  (let [hs (into [] (mapcat #(if (sequential? %) % [%])) handlers)]
    (when (empty? hs)
      (throw (util/argument-error (str (name op) " " (pr-str path) ": at least one handler is required")
                                  {:op op :path path})))
    (doseq [h hs]
      (when-not (handler? h)
        (throw (util/argument-error (str (name op) " " (pr-str path) ": handler must be a function, a middleware descriptor or a vector of those, got " (pr-str h))
                                    {:op op :path path :value h}))))
    hs))

;; ---------------------------------------------------------------------------
;; Paths
;; ---------------------------------------------------------------------------

(defn check-path
  "Validates a route path before it reaches path-to-regexp 8: a string in
   Express 5 syntax, a RegExp, or a non-empty vector of
   those. The three Express 4 spellings are rejected with a clearer
   message than upstream's `Unexpected ( at index 4`: `:id?` (use
   `{/:id}`), `(regexp)` groups inside strings (use a RegExp path), and an
   unnamed `*` (use `*splat`). `+` and `!` are reserved too. Returns the
   path."
  [path]
  (letfn [(bad [msg] (throw (util/argument-error (str "route path " (pr-str path) ": " msg)
                                                 {:arg :path :value path})))]
    (cond
      (string? path)
      (do (when (str/blank? path) (bad "must not be blank"))
          (when-not (str/starts-with? path "/") (bad "must start with /"))
          (when (str/includes? path "?") (bad "the Express 4 optional modifier `?` is not supported by path-to-regexp 8; write /users{/:id} instead"))
          (when (re-find #"[()]" path) (bad "regexp groups inside string paths are not supported by path-to-regexp 8; pass a RegExp path instead"))
          (when (re-find #"\*(?![A-Za-z_$\"])" path) (bad "wildcards must be named in Express 5: use /*splat (a parameter named splat holding a vector of segments)"))
          (when (re-find #"[+!]" path) (bad "`+` and `!` are reserved characters in path-to-regexp 8 (escape with \\\\)"))
          path)
      (regexp? path) path
      (and (vector? path) (seq path)) (do (run! check-path path) path)
      :else (bad "must be a string, a RegExp or a vector of those"))))

(defn- segment-rank
  "Specificity rank of one path segment: 0 literal, 1 parameter, 2 optional
   group, 3 wildcard."
  [seg]
  (cond
    (str/includes? seg "*") 3
    (str/includes? seg "{") 2
    (str/includes? seg ":") 1
    :else 0))

(defn specificity
  "Sort key of a route path for the table form: the vector of segment ranks
   (compared lexicographically, shorter prefixes first when equal) followed
   by the path string. RegExps rank after every string; a vector of paths
   uses its most specific member."
  [path]
  (cond
    (string? path) [(mapv segment-rank (remove str/blank? (str/split path #"/"))) path]
    (vector? path) (first (sort (map specificity path)))
    :else [[9] (str path)]))

(defn- lexicographic-compare
  "Element-wise comparison of two rank vectors; a strict prefix sorts first
   (CLJS `compare` orders vectors by length first, which is not what
   specificity needs)."
  [a b]
  (loop [a (seq a) b (seq b)]
    (cond
      (and (nil? a) (nil? b)) 0
      (nil? a) -1
      (nil? b) 1
      :else (let [c (compare (first a) (first b))]
              (if (zero? c) (recur (next a) (next b)) c)))))

(defn specificity-compare
  "Comparator over two `specificity` keys: rank vectors lexicographically
   (a shorter prefix first), then the path string."
  [[ra pa] [rb pb]]
  (let [c (lexicographic-compare ra rb)]
    (if (zero? c) (compare pa pb) c)))

;; ---------------------------------------------------------------------------
;; Constructors (ordered form)
;; ---------------------------------------------------------------------------

(defn route
  "Generic route entry: `(route :get \"/x\" mw handler)` -> {:op :route
   :method :get :path \"/x\" :handlers [...]}. `method` is one of `methods`
   or :all; handlers may be functions, descriptors or vectors of those."
  [method path & handlers]
  (when-not (or (= :all method) (contains? methods method))
    (throw (util/argument-error (str "route: unknown method " (pr-str method))
                                {:arg :method :value method :allowed (conj methods :all)})))
  {:op :route :method method :path (check-path path) :handlers (flatten-handlers method path handlers)})

(defn get     "GET route entry (see `route`)."     [path & handlers] (apply route :get path handlers))
(defn post    "POST route entry (see `route`)."    [path & handlers] (apply route :post path handlers))
(defn put     "PUT route entry (see `route`)."     [path & handlers] (apply route :put path handlers))
(defn patch   "PATCH route entry (see `route`)."   [path & handlers] (apply route :patch path handlers))
(defn delete  "DELETE route entry (see `route`)."  [path & handlers] (apply route :delete path handlers))
(defn head    "HEAD route entry (see `route`)."    [path & handlers] (apply route :head path handlers))
(defn options "OPTIONS route entry (see `route`)." [path & handlers] (apply route :options path handlers))
(defn query   "QUERY route entry (see `route`)." [path & handlers] (apply route :query path handlers))
(defn all     "Entry matching every method (Express app.all)." [path & handlers] (apply route :all path handlers))

(defn use
  "Middleware entry: `(use mw ...)` mounts at \"/\", `(use \"/api\" mw ...)`
   at a prefix (Express app.use: the prefix is stripped from :url / :path
   and kept in :base-url). Handlers may be functions
   (nil -> next), descriptors, or a nested router entry."
  [& args]
  (let [[path handlers] (if (string? (first args)) [(first args) (rest args)] ["/" args])]
    {:op :use :path (check-path path) :handlers (flatten-handlers :use path handlers)}))

(def router-option-keys
  "Options of a nested router: :merge-params?,
   :case-sensitive?, :strict?."
  #{:merge-params :case-sensitive :strict})

(declare compile)

(defn router
  "Nested router entry mounted at `path`: `(router \"/admin\" {:merge-params?
   true} entries...)` or `(router \"/admin\" entries...)`. Entries are any
   route sources (tables, ordered forms, constructors) compiled in order.
   The table-form equivalent is `{:mount \"/admin\" :opts {..} :routes ...}`."
  [path & more]
  (let [[opts entries] (if (and (map? (first more)) (not (contains? (first more) :op))
                                (not (contains? (first more) :express-cljs/routes))
                                (every? router-option-keys (map util/strip-question-mark (keys (first more)))))
                         [(first more) (rest more)]
                         [{} more])
        opts (util/check-opts! :router router-option-keys (util/normalize-opts opts))]
    {:op :router :path (check-path path) :opts opts :plan (apply compile entries)}))

(defn resource
  "One path with per-method handlers and an automatic 405 for every other
   method (Express app.route(path).get(..).post(..).all(405)):
   `(resource \"/users\" {:get list :post create})`. A `:all` entry replaces
   the default 405 handler (which throws an http-error 405 carrying an
   Allow header)."
  [path method-map]
  (when-not (and (map? method-map) (seq method-map))
    (throw (util/argument-error (str "resource " (pr-str path) ": expected a non-empty map of method -> handler")
                                {:arg :method-map :value method-map})))
  (doseq [m (keys method-map)]
    (when-not (or (= :all m) (contains? methods m))
      (throw (util/argument-error (str "resource " (pr-str path) ": unknown method " (pr-str m))
                                  {:arg :method :value m}))))
  {:op :resource :path (check-path path)
   :methods (into {} (map (fn [[m h]] [m (flatten-handlers m path [h])])) method-map)})

(defn param
  "Parameter pre-loader (Express app.param / router.param):
   `(param \"id\" (fn [req value] ...))` runs before any route whose path
   has `:id`; return nil to continue, `(continue {:user u})` to enrich the
   request map, a response map to answer, or throw (promises honoured)."
  [name f]
  (when-not (and (string? name) (not (str/blank? name)))
    (throw (util/argument-error "param: name must be a non-empty string" {:arg :name :value name})))
  (when-not (fn? f)
    (throw (util/argument-error (str "param " name ": expected a function") {:arg :f :value f})))
  {:op :param :name name :handler f})

(defn error
  "Error-handler entry: `(error f ...)` or `(error \"/api\" f ...)`; each `f`
   is `(fn [err req] ...)` as in `error-handler`. Registered where it
   appears, so put it after the routes it should cover (Express runs error
   middleware in registration order)."
  [& args]
  (let [[path fs] (if (string? (first args)) [(first args) (rest args)] ["/" args])]
    {:op :use :path (check-path path)
     :handlers (mapv #(if (descriptor? %) % (error-handler %)) (flatten-handlers :error path fs))}))

(defn static
  "Static file entry (express.static): `(static \"/static\"
   \"/abs/root\" {:dotfiles :ignore :max-age \"1d\" :index false ...})`.
   Equivalent to `(use \"/static\" (middleware/static root opts))`."
  ([path root] (static path root {}))
  ([path root opts]
   (when-not (and (string? root) (not (str/blank? root)))
     (throw (util/argument-error "static: root must be a non-empty directory path" {:arg :root :value root})))
   {:op :use :path (check-path path)
    :handlers [{:express-cljs/builtin :static :root root :opts (or opts {})}]}))

(defn routes
  "Ordered bundle of entries (tables, constructors, tuples, nested bundles),
   registered in source order."
  [& entries]
  {:express-cljs/routes (vec entries)})

;; ---------------------------------------------------------------------------
;; Compilation
;; ---------------------------------------------------------------------------

(defn- use-entries->ops
  "The :use value of a table -> ops. A flat vector may start with a path
   string that applies to every following handler; an element that is
   itself a vector is `[path? handler ...]`; a map with :routes is a nested
   router; descriptors / fns mount at the table's path."
  [v]
  (let [[base items] (if (string? (first v)) [(first v) (rest v)] ["/" v])]
    (reduce (fn [ops item]
              (cond
                (vector? item) (conj ops (apply use item))
                (and (map? item) (contains? item :routes))
                (conj ops (router (or (:mount item) "/") (or (:opts item) {}) (:routes item)))
                (and (map? item) (contains? item :op)) (conj ops item)
                :else (conj ops (use base item))))
            [] items)))

(defn- table->plan
  "Table form -> ops in the documented kind / specificity order."
  [t]
  (let [ks (set (keys t))
        method-ks (filter #(or (= :all %) (contains? methods %)) ks)
        unknown (remove (into (set table-kinds) (conj methods :all)) ks)]
    (when (seq unknown)
      (throw (util/argument-error (str "route table: unknown key(s) " (pr-str (vec unknown))
                                       "; allowed: method keywords, :all, " (pr-str table-kinds))
                                  {:unknown (set unknown)})))
    (-> []
        (into (map (fn [[n f]] (param n f))) (:param t))
        (into (use-entries->ops (:use t)))
        (into (map (fn [r] (router (or (:mount r) "/") (or (:opts r) {}) (:routes r)))) (:routers t))
        (into (->> (for [m method-ks
                         [path h] (clojure.core/get t m)]
                     [(specificity path) (if (= :all m) 1 0) m path h])
                   (sort (fn [[ka aa _ _ _] [kb ab _ _ _]]
                           (let [c (specificity-compare ka kb)]
                             (if (zero? c) (compare aa ab) c))))
                   (map (fn [[_ _ m path h]] (route m path h)))))
        (into (map (fn [[path mm]] (resource path mm))) (:resource t))
        (into (map #(if (descriptor? %) (use %) (error %))) (:error t)))))

(defn- tuple->op
  "[:get \"/p\" h ...] / [:use \"/p\" mw ...] / [:param \"id\" f] /
   [:router \"/p\" opts? entries...] / [:resource \"/p\" {..}] /
   [:error \"/p\"? f ...] / [:static \"/p\" root opts?] -> op."
  [[kind & args :as tuple]]
  (case kind
    :use (apply use args)
    :param (apply param args)
    :router (apply router args)
    :resource (apply resource args)
    :error (apply error args)
    :static (apply static args)
    (if (or (= :all kind) (contains? methods kind))
      (apply route kind args)
      (throw (util/argument-error (str "route tuple: unknown kind " (pr-str kind) " in " (pr-str tuple))
                                  {:value tuple})))))

(defn op?
  "True for a compiled registration op ({:op :route ...} etc.)."
  [v]
  (and (map? v) (contains? #{:route :use :router :resource :param} (:op v))))

(defn compile
  "Compiles any number of route sources into the flat registration plan
   `core/app` executes: each source may be an op, a `routes` bundle, a
   table, a tuple, or a vector of sources. Pure and total: every validation
   error surfaces here as an argument-error, before any Express call."
  [& sources]
  (into []
        (mapcat (fn [s]
                  (cond
                    (nil? s) []
                    (op? s) [s]
                    (and (map? s) (contains? s :express-cljs/routes)) (apply compile (:express-cljs/routes s))
                    (map? s) (table->plan s)
                    (and (vector? s) (keyword? (first s))) [(tuple->op s)]
                    (sequential? s) (apply compile s)
                    :else (throw (util/argument-error (str "route source must be a table, an entry or a vector of those, got " (pr-str s))
                                                      {:value s})))))
        sources))

(defn plan-paths
  "Introspection: [[method-or-kind path] ...] of a compiled plan in
   registration order, nested routers flattened with their mount prefix.
   Handy for tests and for printing an app's routing table."
  ([plan] (plan-paths "" plan))
  ([prefix plan]
   (into []
         (mapcat (fn [{:keys [op method path methods plan] :as o}]
                   (let [p (if (string? path) (str prefix path) path)]
                     (case op
                       :route [[method p]]
                       :use [[:use p]]
                       :param [[:param (:name o)]]
                       :resource (mapv (fn [m] [m p]) (keys methods))
                       :router (plan-paths (str prefix (when (not= "/" path) path)) plan)))))
         plan)))
