# express-cljs

ClojureScript (Node.js) wrapper of [Express 5](https://expressjs.com) for HTTP
APIs, plus WebSockets through the [`ws`](https://github.com/websockets/ws) package
on the same HTTP server, plus a JSON-RPC 2.0 layer that mounts **one** method
registry over **both** transports (a single `POST /rpc` endpoint and websocket
frames). A complete wrapper of Express's application / router / request / response
surface, written as ClojureScript only.

The authoritative spec is `docs/express-api-inventory.org` (express 5.2.1 and
ws 8.21.3 census against the installed packages and `.d.ts` files, cross-checked
with the official docs, 2026-09-25); the user manual is `docs/express-cljs.org`.
Zero runtime npm dependencies beyond `express` and `ws`; promesa promises for every
async operation; kebab-case maps in and out; websocket messages and connection
events arrive on core.async channels.

## Hard style rules (non-negotiable)

- ClojureScript only at every call site: **no `^js` type hints anywhere, no
  JavaScript interop outside the two binding namespaces** — `express-cljs.core`
  (Express / Node) and `express-cljs.ws` (the `ws` package), both in the `js-invoke`
  + `goog.object` house style. `route`, `middleware` and `rpc` are plain
  ClojureScript calling those two; `util` is pure (strings, keywords, maps).
- Kebab-case keyword maps in (`{:trust-proxy true :json {:limit "1mb"}}`) and out
  (`{:status 200 :headers {:content-type ..} :body ..}`); keys are re-cased
  mechanically, values never are. HTTP header keys are lower-case kebab keywords
  (`:content-type`), never camelCase, never capitalised. User JSON (bodies, RPC
  params and results, query keys) is keywordised as spelled and never re-cased.
- Handlers are functions of a request **map** returning a response **map** (or a
  promise of one); the wrapper does the `req`/`res` translation. Async operations
  return promesa promises; websocket messages and lifecycle events are core.async
  channels (close the channel to unsubscribe). Sends are promises, never channels.
- RPC: one CLJS-native method registry `{name (fn [params ctx])}` served over both
  transports with one error mapping (`util/rpc-error-codes`).
- Errors are `ex-info` values with `:type` in `#{:express-cljs/http-error
  :express-cljs/server-error :express-cljs/ws-error :express-cljs/rpc-error
  :express-cljs/argument-error}` (`util/error-data`, `util/error-status`).
- Zero runtime npm dependencies beyond `express` and `ws`. Third-party middleware
  (cookie-parser, multer, cors, helmet, compression, session) is out of scope and
  never added silently; the wrapper exposes a way to mount a caller-supplied one.

## Status

| Phase | Content | State |
|-------|---------|-------|
| 0 | scaffold: shadow-cljs `:node-test` / `:dev` builds, `run-tests.sh`, `util` (kebab/camel + app-setting + header-name translation, option whitelists, error constructors, status mapping) | done |
| 1 | API inventory (`docs/express-api-inventory.org`): express 5 vs 4 decision, ws integration decision, routing / middleware / req / res / lifecycle / ws census, RPC design space, error taxonomy, pitfalls, numbered uncertainties, phase outline | done |
| 2 | `core` (the Express / Node binding: app + server lifecycle, req→map / map→res translation, cookies incl. Node-crypto signing, body parsers, static files, error chain + renderer, graceful `close!` with close hooks, the upgrade-request seam) | done |
| 3 | `route` (routing as data: table form with specificity ordering + ordered form, nested routers, resources with 405, params, flow-control sentinels) and `middleware` (json / urlencoded / text / raw / static descriptors, `wrap-*` Ring-style helpers, request-id, cors) | done |
| 4 | `ws` (the `ws` binding: one upgrade listener per server, pre-handshake `:authenticate` with real 401/403, connection maps with IN/OUT core.async channels, send / broadcast / topics / ping / close codes, receive + send backpressure, unref'd keepalive, `close!` choreography with per-socket deadline, `connect!` client) and `rpc` (JSON-RPC 2.0 frames, registry, `call!` dispatch with timeouts, `http-routes` single endpoint with transport-status table and opt-in `:status-map?`, `serve-ws!`, notifications both ways, `client`) | done |
| 5 | localhost smoke (`dev/express_cljs/smoke.cljs`, `:smoke` build): one real 127.0.0.1 server, HTTP + streaming + close with keep-alive + websockets incl. the client-side 1009 observation + RPC over both transports + authentication + full shutdown; 67 checks, process exits by itself | done |
| 6 | user manual (`docs/express-cljs.org`, script-checked API reference) + README | done |

`./run-tests.sh` — 127 tests / 1027 assertions (`util`, `route`, `middleware`, `core`,
`ws`, `rpc`), no network beyond loopback: server tests start real servers on ephemeral
127.0.0.1 ports (websocket tests connect with `ws/connect!`) and close them (about 4 s
of test time after the compile). Set `EXPRESS_CLJS_TRACE=1` to print each async test as
it starts and finishes. `npx shadow-cljs compile smoke && node target/smoke.js` runs the
live smoke (numbered PASS/FAIL lines, tally, exit code, and a liveness line proving the
process drained on its own). `dev/api-index.sh --check` verifies that every public var
has a docstring and is referenced in the manual.

## Quick look

```clojure
(require '[express-cljs.core :as core]
         '[express-cljs.route :as route]
         '[express-cljs.middleware :as mw]
         '[express-cljs.ws :as ws]
         '[express-cljs.rpc :as rpc]
         '[promesa.core :as p])

(def methods
  (rpc/registry {"math/add"  (fn [{:keys [a b]} _ctx] (+ a b))
                 "jobs/run"  (fn [params {:keys [notify! id]}]        ; notify! is nil over HTTP
                               (p/do (when notify! (notify! "jobs/progress" {:id id :pct 50}))
                                     {:done true}))}))

(def app
  (core/app {:env :production :trust-proxy :loopback :cookie-secret "s3cret" :json true}
    {:use  [(mw/request-id)]
     :get  {"/users/me"  (fn [req] {:body {:id (:request-id req)}})
            "/users/:id" (fn [{:keys [params]}]
                           (p/let [u (find-user (:id params))]
                             (if u {:body u} (throw (core/http-error 404 "no such user")))))
            "/files/*splat" (fn [{:keys [params]}] {:send-file {:path (clojure.string/join "/" (:splat params)) :root "/srv/files"}})}
     :post {"/users" (fn [{:keys [body]}] {:status 201 :body (create-user! body)})}
     :routers [{:mount "/admin" :routes {:get {"/" (mw/wrap-bearer-auth admin-home {:verify check-token})}}}]
     :error [(fn [err req] nil)]}                         ; nil = let the wrapper render it
    (rpc/http-routes methods {:context (fn [req] {:user (get-in req [:headers :x-user])})})))

(p/let [server (core/listen! app {:port 8080 :host "127.0.0.1"})
        wsv    (ws/serve! server {:path "/ws" :authenticate (fn [{:keys [headers]}]
                                                             (when-let [t (:x-auth headers)] {:user (verify t)}))})]
  (rpc/serve-ws! wsv methods)                             ; the same registry over websocket frames
  ;; ... later:
  (core/close! server {:timeout-ms 5000 :ws-close-timeout-ms 2000}))
```

Handlers are `(fn [req-map] -> response-map | nil | promise)`. `nil` falls through to the
next route; `(route/continue {:user u})` continues while enriching the request map;
`(route/next-route)` / `(route/next-router)` are Express's `next('route')` /
`next('router')`. Response maps: `{:status :headers :body :type :json :cookies
:clear-cookies :vary :links :attachment :redirect :send-file :download :format :end}`.
Websocket connections are maps with `(ws/messages conn)` / `(ws/out conn)` channels and
`ws/send!` / `ws/close!` / `ws/ping!` promises; JSON-RPC frames are answered on the same
connection, notifications flow both ways (`rpc/notify!`).

## Documentation

- [`docs/express-cljs.org`](docs/express-cljs.org) — the user manual: quick start,
  core (app options, `listen!`, the `close!` choreography, request and response maps
  incl. streaming, cookies, static files), routing as data, middleware helpers,
  websockets, the RPC layer (registry, frames, error-code table, transport statuses,
  context injection, notifications), error handling, design decisions, testing and
  the smoke, and a script-checked API reference.
- [`docs/express-api-inventory.org`](docs/express-api-inventory.org) — the research the
  library is built on: version and websocket-package decisions with evidence, the
  routing / middleware / request / response / lifecycle / ws census, the RPC design
  space, error taxonomy, pitfalls, the numbered uncertainties with the house position
  on each, and the corrections log (section numbers quoted in docstrings point here).
- Namespace and function docstrings in `src/express_cljs/` are the API truth.

## Dependency note: promesa

`src/promesa/impl/promise.js` is a copy of promesa 12.0.1's ClojureScript promise
implementation with a one-line fix (marked `express-cljs patch`): upstream lets a
synchronous throw inside one `p/do` / `p/let` body flip the drain state for every later
continuation queued on its shared null-promise singleton in the same tick, and those
continuations then resolve with `nil` without running — concurrent handlers could be
silently skipped. The source path precedes the jar on the classpath, so the copy shadows
the jar's file; `util-test/promesa-drain-is-not-poisoned-by-a-throwing-body` pins the
behaviour. Remove the file once upstream ships a fix (see the design-decisions chapter
of the manual).

## Development

```
./run-tests.sh                                  # exec's node at the end — never chain it mid-command
npx shadow-cljs compile smoke && node target/smoke.js
dev/api-index.sh --check                        # docstrings + manual coverage
npx shadow-cljs watch dev && node target/dev.js # REPL runtime
```
