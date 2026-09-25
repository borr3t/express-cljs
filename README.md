# express-cljs

ClojureScript (Node.js) wrapper of [Express 5](https://expressjs.com) for HTTP
APIs, plus WebSockets through the [`ws`](https://github.com/websockets/ws) package
on the same HTTP server, plus an RPC layer that mounts one method registry over
**both** transports (a single `POST /rpc` endpoint and websocket frames). A
complete wrapper of Express's application / router / request / response surface,
written as ClojureScript only.

The authoritative spec is `docs/express-api-inventory.org` (express 5.2.1 and
ws 8.21.3 census against the installed packages and `.d.ts` files, cross-checked
with the official docs, 2026-09-25). Zero runtime npm dependencies beyond
`express` and `ws`; promesa promises for every async operation; kebab-case maps
in and out; websocket messages arrive on core.async channels.

## Hard style rules (non-negotiable)

- ClojureScript only at every call site: **no `^js` type hints anywhere, no
  JavaScript interop outside the single binding namespace** (`express-cljs.core`,
  `js-invoke` + `goog.object` house style). `route`, `ws`, `rpc` and `middleware`
  are plain ClojureScript calling `core`; `util` is pure (strings, keywords, maps).
- Kebab-case keyword maps in (`{:trust-proxy true :json {:limit "1mb"}}`) and out
  (`{:status 200 :headers {:content-type ..} :body ..}`); keys are re-cased
  mechanically, values never are. HTTP header keys are lower-case kebab keywords
  (`:content-type`), never camelCase, never capitalised.
- Handlers are functions of a request **map** returning a response **map** (or a
  promise of one); the wrapper does the `req`/`res` translation. Async operations
  return promesa promises; websocket messages and lifecycle events are core.async
  channels (close the channel to unsubscribe).
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
| 1 | API inventory (`docs/express-api-inventory.org`): express 5 vs 4 decision, ws integration decision, routing / middleware / req / res / lifecycle / ws census, RPC design space, error taxonomy, pitfalls, numbered uncertainties, Phase 2–4 outline | done |
| 2 | `core` (the only interop namespace: app + server lifecycle, req→map / map→res translation, mounting, error handler, graceful shutdown), `route` (routing as data), `middleware` (built-ins json / urlencoded / text / raw / static as options, composable helpers) | planned |
| 3 | `ws` (websocket server on the same HTTP server: per-connection core.async channels, send / broadcast / close, keepalive with unref'd timers, backpressure) | planned |
| 4 | `rpc` (method registry over HTTP POST + ws frames, request-id correlation, error mapping, auth context injection, push events) | planned |
| 5 | live smoke (`dev/express_cljs/smoke.cljs`: a real server on an ephemeral port exercised with Node `fetch` and a `ws` client, clean process exit) | planned |
| 6 | user manual (`docs/express-cljs.org`) + README | planned |

`./run-tests.sh` — 14 tests / 118 assertions (Phase 0: `util` only), no network.

## Documentation

- [`docs/express-api-inventory.org`](docs/express-api-inventory.org) — the research the
  library is built on: version and websocket-package decisions with evidence, the
  routing / middleware / request / response / lifecycle / ws census, the RPC design
  space, error taxonomy, pitfalls, and the numbered uncertainties with the house
  position on each (section numbers quoted in docstrings point here).
- Namespace and function docstrings in `src/express_cljs/` are the API truth.

## Development

```
./run-tests.sh                                  # exec's node at the end — never chain it mid-command
npx shadow-cljs watch dev && node target/dev.js # REPL runtime
```
