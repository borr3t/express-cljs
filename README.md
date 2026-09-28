# express-cljs

An idiomatic ClojureScript (Node.js) wrapper of [Express 5](https://expressjs.com) for
HTTP APIs, of the [`ws`](https://github.com/websockets/ws) package for WebSockets on the
same HTTP server, and a JSON-RPC 2.0 layer that serves **one** method registry over
**both** transports. Handlers are functions of a request map that return a response map,
routes are data, every asynchronous operation returns a
[promesa](https://github.com/funcool/promesa) promise, and websocket messages arrive on
`core.async` channels. The only runtime npm dependencies are `express` and `ws`.

## Requirements

Node.js 22+, Java 11+ for shadow-cljs. The library is consumed as a source dependency
(git or `:local/root`) by another shadow-cljs Node app, which has `express` and `ws` in
its own `package.json`; `deps.edn` declares ClojureScript, core.async and promesa.

## Quick start (offline)

The quick start serves an HTTP API **over a stub** — an atom of users stands where a
database would — on a free port of the loopback interface, makes three requests of it
and closes it. Nothing leaves the machine.

```bash
npm install
npx shadow-cljs compile examples && node target/examples.js
```

That runs [`dev/express_cljs/examples.cljs`](dev/express_cljs/examples.cljs), which holds
every example of this page. Its namespace form:

```clojure
(ns express-cljs.examples
  (:refer-clojure :exclude [methods])
  (:require [cljs.core.async :as async :refer [<! go-loop]]
            [clojure.string :as str]
            [express-cljs.core :as core]
            [express-cljs.middleware :as mw]
            [express-cljs.rpc :as rpc]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [promesa.core :as p]))
```

The harness: a server that lives as long as a function runs, and a client for it.

```clojure
(defn with-server
  "Listens `app` on a free port of the loopback interface, runs
   `(f server url)` and closes the server, whatever `f` did. Resolves
   with what `f` resolved with."
  [app f]
  (p/let [server  (core/listen! app {:port 0 :host "127.0.0.1"})
          url     (str "http://127.0.0.1:" (:port (core/address server)))
          outcome (-> (p/do (f server url))
                      (p/then (fn [value] {:value value}))
                      (p/catch (fn [e] {:error e})))
          _       (core/close! server {:timeout-ms 1000})]
    (if-let [e (:error outcome)]
      (throw e)
      (:value outcome))))

(defn request!
  "One HTTP request, made with Node's fetch. `opts`: :method (default
   :get), :headers (a map of kebab keywords), :body (a map, sent as JSON),
   :show (the response headers to report). Resolves with {:status :body},
   and :headers when :show is given; a JSON body arrives parsed."
  ([url] (request! url {}))
  ([url {:keys [method headers body show]}]
   (p/let [init {:method  (str/upper-case (name (or method :get)))
                 :headers (cond-> (util/->wire-headers (or headers {}))
                            body (assoc "content-type" "application/json"))}
           init (cond-> init
                  body (assoc :body (js/JSON.stringify (clj->js body))))
           resp (js/fetch url (clj->js init))
           text (.text resp)
           json? (str/includes? (str (.get (.-headers resp) "content-type")) "json")]
     (cond-> {:status (.-status resp)
              :body   (if json?
                        (js->clj (js/JSON.parse text) :keywordize-keys true)
                        text)}
       show (assoc :headers (into {} (map (fn [h] [h (.get (.-headers resp) (name h))])) show))))))
```

`request!` is the client, and the only JavaScript interop of the page: the library
itself is used without any. The API, and three requests of it:

```clojure
(defn api
  "An HTTP API over `users`, an atom of {id user}: the stub that stands
   where a database would."
  [users]
  (core/app
   {:env :production :json true}
   {:get  {"/users/:id" (fn [{:keys [params]}]
                          (if-let [user (get @users (:id params))]
                            {:body user}
                            (throw (core/http-error 404 "no such user"))))}
    :post {"/users"     (fn [{:keys [body]}]
                          (let [id   (str (inc (count @users)))
                                user (assoc body :id id)]
                            (swap! users assoc id user)
                            {:status  201
                             :headers {:location (str "/users/" id)}
                             :body    user}))}}))

(defn quick-start []
  (with-server (api (atom {"1" {:id "1" :name "Ada"}}))
    (fn [_server url]
      (p/let [created (request! (str url "/users") {:method :post
                                                    :body   {:name "Grace"}
                                                    :show   [:location]})
              found   (request! (str url "/users/2"))
              missing (request! (str url "/users/9"))]
        {:created created
         :found   found
         :missing missing}))))
```

`(quick-start)` resolves with:

```clojure
{:created {:status  201
           :body    {:name "Grace" :id "2"}
           :headers {:location "/users/2"}}
 :found   {:status 200 :body {:name "Grace" :id "2"}}
 :missing {:status 404 :body {:error {:status 404 :message "no such user"}}}}
```

A handler takes the request as a map and returns the response as a map, or a promise
of one. A map or vector `:body` is sent as JSON; `:json true` in the options of the app
parses JSON request bodies. What a handler throws is answered by the library: an
`http-error` with its status and message, anything else as a 500.

### Going live

The same app serves real clients once it listens where they can reach it. **The port,
the interface, what stands behind the handlers and every secret are yours**: the
library ships no default credential, and the values of this page (`"token-of-ada"`,
`"a secret of yours"`) are placeholders to replace, never to deploy.

```clojure
(defn main [& _]
  (p/let [server (core/listen! (api (atom {})) {:port 8080 :host "127.0.0.1"})]
    (println "listening on" (core/address server))
    (js/process.on "SIGTERM" #(core/close! server {:timeout-ms 5000}))))
```

```bash
curl -i -X POST http://127.0.0.1:8080/users -H 'content-type: application/json' -d '{"name":"Grace"}'
curl -i http://127.0.0.1:8080/users/1
```

Without `:host` Node listens on every interface of the machine; name the interface you
mean. Behind a reverse proxy set `:trust-proxy`, or `:ip` and `:protocol` of the request
map describe the proxy. A `:cookie-secret` belongs in the environment of the process,
read at startup and never logged.

## Scenarios

Each scenario is a function of `dev/express_cljs/examples.cljs`, shown with the result
it returns against its own server on the loopback interface.

### Routes as data, guarded by middleware

```clojure
(def tokens
  "The stub that stands where a token store would."
  {"token-of-ada" {:user "ada" :admin? true}
   "token-of-bob" {:user "bob"}})

(defn signed-in
  "`handler`, for requests that carry a bearer token of `tokens`; the
   entry of the token reaches the handler as :auth."
  [handler]
  (mw/wrap-bearer-auth handler {:verify (fn [token] (get tokens token))}))

(defn admins-only [handler]
  (fn [{:keys [auth] :as req}]
    (if (:admin? auth)
      (handler req)
      (throw (core/http-error 403 "admins only")))))

(def guarded-api
  {:use      [(mw/request-id)]
   :get      {"/me" (signed-in (fn [{:keys [auth]}] {:body auth}))}
   :routers  [{:mount  "/admin"
               :routes {:get {"/stats" (signed-in (admins-only (fn [_] {:body {:users 2}})))}}}]
   :resource {"/health" {:get (fn [_] {:body {:ok true}})}}})

(defn guard []
  (with-server (core/app {:env :production} guarded-api)
    (fn [_server url]
      (let [as (fn [token] {:headers {:authorization (str "Bearer " token)}})]
        (p/let [anonymous (request! (str url "/me") {:show [:www-authenticate]})
                bob       (request! (str url "/me") (as "token-of-bob"))
                bob-admin (request! (str url "/admin/stats") (as "token-of-bob"))
                ada-admin (request! (str url "/admin/stats") (as "token-of-ada"))
                deleted   (request! (str url "/health") {:method :delete :show [:allow]})]
          {:anonymous anonymous
           :bob       bob
           :bob-admin bob-admin
           :ada-admin ada-admin
           :deleted   deleted})))))
```

`(guard)` resolves with:

```clojure
{:anonymous {:status  401
             :body    {:error {:status 401 :message "bearer token required" :reason "unauthorized"}}
             :headers {:www-authenticate "Bearer realm=\"api\""}}
 :bob       {:status 200 :body {:user "bob"}}
 :bob-admin {:status 403 :body {:error {:status 403 :message "admins only"}}}
 :ada-admin {:status 200 :body {:users 2}}
 :deleted   {:status  405
             :body    {:error {:status 405 :message "Method Not Allowed" :reason "method-not-allowed"}}
             :headers {:allow "GET, HEAD"}}}
```

The routing table is a map, so it has no order of its own: the library registers
literal paths before parameters before wildcards, and `/users/me` is never shadowed by
`/users/:id`. A `:resource` answers the methods it does not have with 405 and `Allow`.
Middleware in the Ring style wraps a handler and never sees `req` or `res`.

### A chat over websockets

```clojure
(defn serve-chat!
  "A chat at /chat of `server`. A client names itself in the query of the
   handshake; without a name the handshake is refused with HTTP 401. What
   a client says goes to everyone else in the lobby."
  [server]
  (let [chat (ws/serve! server {:path "/chat"
                                :authenticate (fn [{:keys [query]}]
                                                (when-let [name (:name query)]
                                                  {:name name}))})]
    (ws/on-open! chat
                 (fn [conn]
                   (let [name (:name (ws/ctx conn))]
                     (ws/join! chat conn "lobby")
                     (ws/send-json! conn {:welcome name})
                     (go-loop []
                       (when-let [message (<! (ws/messages conn))]
                         (ws/emit! chat "lobby"
                                   {:json {:from name :text (:data message)}}
                                   {:except conn})
                         (recur))))))
    chat))

(defn heard
  "Promise of the next message of `conn`, parsed."
  [conn]
  (p/create (fn [resolve _]
              (async/take! (ws/messages conn)
                           (fn [message] (resolve (some-> message ws/json)))))))

(defn chat []
  (with-server (core/app {:env :production})
    (fn [server url]
      (let [chat-url (str (str/replace url "http://" "ws://") "/chat")]
        (serve-chat! server)
        (p/let [ada      (ws/connect! (str chat-url "?name=ada"))
                welcome  (heard ada)
                bob      (ws/connect! (str chat-url "?name=bob"))
                _        (heard bob)
                _        (ws/send! ada "hello")
                bob-hears (heard bob)
                nameless (-> (ws/connect! chat-url)
                             (p/then (fn [_] :accepted))
                             (p/catch (fn [e] (select-keys (ex-data e) [:type :code :status]))))]
          {:welcome   welcome
           :bob-hears bob-hears
           :nameless  nameless})))))
```

`(chat)` resolves with:

```clojure
{:welcome   {:welcome "ada"}
 :bob-hears {:from "ada" :text "hello"}
 :nameless  {:type :express-cljs/ws-error :code :unexpected-response :status 401}}
```

`:authenticate` runs before the handshake, on the same request map a route handler
sees, so a client that is refused gets a real HTTP status and never a socket. What it
returns is the `:ctx` of the connection. `core/close!` closes every websocket with
code 1001 before it stops the server, which is why `chat` closes nothing itself.

### One RPC registry, two transports

```clojure
(def methods
  (rpc/registry
   {"math/add" (fn [{:keys [a b]} _ctx]
                 (when-not (and (number? a) (number? b))
                   (throw (rpc/error :invalid-params "a and b are numbers")))
                 (+ a b))
    "jobs/run" (fn [{:keys [steps]} {:keys [notify! id]}]
                 ;; notify! is nil over HTTP: progress needs a connection
                 (p/loop [step 1]
                   (if (> step steps)
                     {:done steps}
                     (p/let [_ (when notify!
                                 (notify! "jobs/progress" {:id id :step step}))]
                       (p/recur (inc step))))))}))

(defn rpc []
  (with-server (core/app {:env :production} (rpc/http-routes methods))
    (fn [server url]
      (rpc/serve-ws! (ws/serve! server {:path "/rpc"}) methods)
      (let [post   (fn [frame] (request! (str url "/rpc") {:method :post :body frame}))
            ws-url (str (str/replace url "http://" "ws://") "/rpc")]
        (p/let [over-http (post {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 1 :b 2}})
                refused   (post {:jsonrpc "2.0" :id 2 :method "math/add" :params {:a 1}})
                conn      (ws/connect! ws-url)
                client    (rpc/client conn)
                call!     (:call! client)
                over-ws   (call! "math/add" {:a 1 :b 2})
                job       (call! "jobs/run" {:steps 2})
                progress  (p/all (repeatedly 2 #(p/create
                                                 (fn [resolve _]
                                                   (async/take! (:notifications client) resolve)))))]
          {:over-http over-http
           :refused   refused
           :over-ws   over-ws
           :progress  (mapv :params progress)
           :job       job})))))
```

`(rpc)` resolves with:

```clojure
{:over-http {:status 200 :body {:jsonrpc "2.0" :id 1 :result 3}}
 :refused   {:status 200
             :body   {:jsonrpc "2.0" :id 2 :error {:code -32602 :message "a and b are numbers"}}}
 :over-ws   3
 :progress  [{:id 2 :step 1} {:id 2 :step 2}]
 :job       {:done 2}}
```

Over HTTP the registry is one endpoint, `POST /rpc`; over a websocket it is text
frames. The frames are the same JSON-RPC 2.0 documents on both. A method that refuses
answers an error frame with HTTP 200: the status of the transport says the request was
answered, not what the answer is (`:status-map? true` maps method errors to HTTP
statuses for clients that want them). Notifications travel both ways over a websocket
and are how a method streams.

## Pure helpers

No server, no network. `handler` is any handler, `(fn [_req] {:body "ok"})`:

```clojure
(route/plan-paths (route/compile {:get {"/files/*path" handler "/users/:id" handler "/users/me" handler}}))
;; => [[:get "/users/me"] [:get "/users/:id"] [:get "/files/*path"]]

(core/parse-cookies "sid=abc; theme=dark")
;; => {:sid "abc" :theme "dark"}

(core/cookie-sign "abc" "a secret of yours")
;; => "abc.4DZonNBp9SYRcW/5DU2TZmU041qknXxCRgjR5IZ6QjQ"

(core/cookie-unsign "abc.4DZonNBp9SYRcW/5DU2TZmU041qknXxCRgjR5IZ6QjQ" "a secret of yours")
;; => "abc"

(core/fresh? {:headers {:if-none-match "\"v1\""}} {:etag "\"v1\""})
;; => true

(util/->wire-headers {:content-type "text/plain" :x-trace nil})
;; => {"content-type" "text/plain"}

(util/error-status (core/http-error 404 "no such user"))
;; => 404

(util/error-status (rpc/error :timeout "too slow"))
;; => 504

(rpc/request-frame 1 "math/add" {:a 1 :b 2})
;; => {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 1 :b 2}}

(rpc/error-frame 1 :method-not-found "method not found: nope")
;; => {:jsonrpc "2.0" :id 1 :error {:code -32601 :message "method not found: nope"}}

(ws/valid-close-code? 4001)
;; => true

(ws/valid-close-code? 1006)
;; => false
```

## Error handling

Every error the library raises is an `ex-info` whose data carries a `:type`:

| `:type`                        | when                                                         | data                                     |
|--------------------------------|--------------------------------------------------------------|------------------------------------------|
| `:express-cljs/http-error`     | a handler, a middleware or a body parser refuses a request   | `:status`, `:message`, `:reason`, `:headers` |
| `:express-cljs/server-error`   | the server cannot listen or close; a response map is malformed | `:code`, `:message`, `:op`             |
| `:express-cljs/ws-error`       | a handshake is refused, a send or a close fails              | `:code`, `:status`, `:close-code`        |
| `:express-cljs/rpc-error`      | a method refuses, is unknown or is too slow                  | `:code`, `:message`, `:data`, `:id`      |
| `:express-cljs/argument-error` | an argument is invalid: found before anything is sent upstream | `:arg`, `:value`                       |

Inside a handler an error is **thrown**, and the library answers the client: the
quick start shows the 404. Outside a handler an error is a **rejected promise**, and
the caller decides:

```clojure
(defn outcome
  "What became of `promise`: its value, or its failure as data."
  [promise]
  (-> promise
      (p/then (fn [value] {:ok? true :value value}))
      (p/catch (fn [e]
                 (let [{:keys [type code status]} (ex-data e)]
                   (case type
                     :express-cljs/server-error   {:ok? false :why :server :code code}
                     :express-cljs/ws-error       {:ok? false :why :websocket :code code :status status}
                     :express-cljs/rpc-error      {:ok? false :why :rpc :code code}
                     :express-cljs/http-error     {:ok? false :why :http :status status}
                     :express-cljs/argument-error {:ok? false :why :argument}
                     (throw e)))))))

(defn errors []
  (with-server (core/app {:env :production} (rpc/http-routes methods))
    (fn [server url]
      (rpc/serve-ws! (ws/serve! server {:path "/rpc"}) methods)
      (p/let [port       (:port (core/address server))
              conn       (ws/connect! (str "ws://127.0.0.1:" port "/rpc"))
              call!      (:call! (rpc/client conn))
              port-taken (outcome (core/listen! (core/app {}) {:port port :host "127.0.0.1"}))
              no-socket  (outcome (ws/connect! (str "ws://127.0.0.1:" port "/nowhere")))
              bad-params (outcome (call! "math/add" {:a 1}))
              bad-route  (outcome (p/do (core/app {} {:get {"/files/*" (fn [_] {:body "file"})}})))
              added      (outcome (call! "math/add" {:a 1 :b 2}))]
        {:port-taken port-taken
         :no-socket  no-socket
         :bad-params bad-params
         :bad-route  bad-route
         :added      added}))))
```

`(errors)` resolves with:

```clojure
{:port-taken {:ok? false :why :server :code :eaddrinuse}
 :no-socket  {:ok? false :why :websocket :code :unexpected-response :status 404}
 :bad-params {:ok? false :why :rpc :code :invalid-params}
 :bad-route  {:ok? false :why :argument}
 :added      {:ok? true :value 3}}
```

`"/files/*"` is the Express 4 spelling of a wildcard; Express 5 names it,
`"/files/*path"`, and the library says so before the route reaches Express. In
production (`:env :production`) the message of an error that is not an `http-error`
stays on the server: the client reads `Internal Server Error`.

## Features

* **Handlers are functions of data** — `(fn [request-map] -> response-map | nil |
  promise)`; `nil` passes the request on to the next route.
* **Routes are data** — a table keyed by method, ordered by specificity, or an ordered
  vector of entries; nested routers, resources with 405 and `Allow`, parameter loaders.
* **Flow control by return value** — `route/continue` passes on and adds keys to the
  request map, `route/next-route` and `route/next-router` are Express's `next('route')`
  and `next('router')`.
* **One response map** — status, headers, cookies, JSON, redirects, files, downloads,
  content negotiation, and streaming of any Node Readable as `:body`.
* **Cookies without cookie-parser** — parsed from the header, signed and verified with
  Node's `crypto`, byte-compatible with Express's signed cookies.
* **Body parsers and static files** — Express's own `json`, `urlencoded`, `text`, `raw`
  and `static`, as validated option maps.
* **Middleware in the Ring style** — bearer authentication, CORS, JSON enforcement,
  required headers, logging, timing, error conversion; composable.
* **Caller-supplied Express middleware** — `route/upstream` and `core/mount-upstream!`
  mount a middleware function you required yourself (helmet, multer, compression); the
  library adds none on its own.
* **WebSockets on the same server** — authentication before the handshake, a channel of
  messages per connection, topics, broadcast, ping, close codes, backpressure both
  ways, keepalive that never keeps the process alive, and a client.
* **JSON-RPC 2.0 over both transports** — one registry, one error table, timeouts per
  method, context injection, notifications both ways, and a client.
* **Graceful shutdown** — `core/close!` closes the websockets, stops accepting, waits
  for requests in flight up to a deadline, then cuts them; the process exits on its
  own afterwards.
* **Structured errors** — every error is an `ex-info` with a `:type`.
* **No interop at call sites** — the public API carries no `^js` hints; JavaScript
  interop lives in two namespaces, `core` and `ws`.

## Limits and settings

The library sets no limit the caller cannot replace. Each of these has a default, and
each is an option of the function named:

| where             | key                                    | default                 | meaning                                              |
|-------------------|----------------------------------------|-------------------------|------------------------------------------------------|
| `core/app`        | `:json`, `:urlencoded`, `:text`, `:raw` | off                    | the body parsers mounted before the routes           |
| `core/app`        | `:json-limit`                          | `"100kb"`               | the largest JSON request body                        |
| `core/app`        | `:trust-proxy`                         | `false`                 | which proxies `:ip` and `:protocol` believe          |
| `core/app`        | `:cookie-secret`                       | none                    | the secret of signed cookies: yours                  |
| `core/app`        | `:expose-errors?`                      | off in production       | whether a client reads the message of a 500          |
| `core/listen!`    | `:port`, `:host`                       | a free port, every interface | where the server listens                        |
| `core/listen!`    | `:request-timeout-ms`, `:headers-timeout-ms`, `:keep-alive-timeout-ms` | 300000, 60000, 5000 | the timeouts of Node's HTTP server |
| `core/close!`     | `:timeout-ms`                          | 5000                    | how long requests in flight may finish               |
| `core/close!`     | `:ws-close-timeout-ms`                 | 2000                    | how long a websocket may take to close               |
| `ws/serve!`       | `:max-payload`                         | 1 MiB                   | the largest message; a larger one closes with 1009   |
| `ws/serve!`       | `:keepalive-ms`                        | 30000                   | the ping period; `false` disables it                 |
| `ws/serve!`       | `:max-buffered-bytes`                  | 8 MiB                   | unsent bytes above which `send!` rejects             |
| `ws/serve!`       | `:message-buffer`                      | 64                      | messages held per connection; a full buffer pauses the socket |
| `ws/connect!`     | `:timeout-ms`                          | 5000                    | the deadline of the handshake                        |
| `rpc/http-routes` | `:limit`                               | `"256kb"`               | the largest request frame                            |
| `rpc/http-routes` | `:status-map?`                         | `false`                 | method errors as HTTP statuses                       |
| `rpc/registry`    | `:timeout-ms` of a method              | none                    | the deadline of a call; answers -32003               |
| `rpc/client`      | `:timeout-ms`                          | 10000                   | how long a call waits for its answer                 |

## Documentation

* [`docs/express-cljs.org`](docs/express-cljs.org) — the manual: the app and the
  server, the request and response maps, routing as data, middleware, websockets, the
  RPC layer with its error table, error handling, and the reference of every public
  function.
* [`docs/express-api-inventory.org`](docs/express-api-inventory.org) — the surface of
  Express 5 and of `ws` that the library wraps, with the source each fact was taken
  from.
* The docstrings of `src/express_cljs/` are the reference of each function.

## Dependency note: promesa

`src/promesa/impl/promise.js` is a copy of the promise implementation of promesa
12.0.1 with a one-line fix, marked `express-cljs patch`. In the original a synchronous
throw inside one `p/do` or `p/let` body changes the state of every later continuation
queued in the same tick, and those continuations resolve with `nil` without running:
of two concurrent handlers, one could be skipped. The source path precedes the jar on
the classpath, so the copy is the one compiled, in this library and in an app that
depends on it. The file goes once promesa ships the fix.

## Layout

```
src/express_cljs/
  core.cljs        the Express / Node binding: app, server lifecycle, request and response maps, cookies, errors
  ws.cljs          the ws binding: endpoints, connections, channels, topics, keepalive, shutdown, client
  route.cljs       routing as data: tables, ordered entries, routers, resources, flow control (pure)
  middleware.cljs  body parser and static descriptors, Ring-style wrappers (pure)
  rpc.cljs         JSON-RPC 2.0: frames, registry, dispatch, HTTP routes, websocket serving, client
  util.cljs        case translation, header policy, option validation, error constructors (pure)
src/promesa/impl/
  promise.js       promesa 12.0.1 with a one-line fix
dev/express_cljs/
  examples.cljs    the examples of this page
  smoke.cljs       one server, every feature, end to end on the loopback interface
docs/
  express-cljs.org            the manual
  express-api-inventory.org   the API inventory
```

## Development

```bash
npm install
./run-tests.sh     # the publicity gate, then the unit suite; loopback only
npx shadow-cljs compile examples && node target/examples.js   # the examples of this page
npx shadow-cljs compile smoke && node target/smoke.js         # the smoke; loopback only
dev/api-index.sh --check                                      # every public function has a docstring and stands in the manual
```

The suite starts real servers on free ports of 127.0.0.1 and closes them; it covers
the translation of requests and responses, routing, middleware, websockets, the RPC
layer over both transports, and the examples of this page: it fails when this page
shows code or a result that the examples do not produce. `./run-tests.sh` ends by
replacing its shell with node, so nothing can follow it on the same command line.
`EXPRESS_CLJS_TRACE=1` prints each asynchronous test as it starts and ends.
