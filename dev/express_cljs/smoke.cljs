(ns express-cljs.smoke
  "Phase 5 localhost smoke (`npx shadow-cljs compile smoke && node
   target/smoke.js`): one real 127.0.0.1 server serving HTTP routes, three
   websocket endpoints and the RPC registry over both transports, driven by
   Node's fetch and the `ws` client through the test-util / ws bindings (no
   interop here). Prints one numbered PASS / FAIL line per check, a tally,
   sets the exit code — and never calls process.exit: the process has to
   drain by itself once `core/close!` resolved, which is the liveness
   evidence (the exit hook prints the elapsed time and the active resources
   at exit)."
  (:refer-clojure :exclude [methods])
  (:require [cljs.core.async :as async]
            [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.middleware :as mw]
            [express-cljs.route :as route]
            [express-cljs.rpc :as rpc]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [express-cljs.test-util :as tu]))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(def results (atom []))
(def section (atom ""))

(defn check!
  "Records and prints one numbered check."
  [ok? label & [detail]]
  (let [n (inc (count @results))
        ok? (boolean ok?)]
    (swap! results conj {:n n :ok? ok? :label label :section @section})
    (println (str (if ok? "PASS" "FAIL") " " (if (< n 10) " " "") n "  [" @section "] " label
                  (when (and (not ok?) detail) (str " -- " (pr-str detail)))))
    ok?))

(defn section! [name]
  (reset! section name)
  (println (str "\n== " name)))

(defn expect!
  "check! with expected / actual shown on failure."
  [label expected actual]
  (check! (= expected actual) label {:expected expected :actual actual}))

;; ---------------------------------------------------------------------------
;; The application under test
;; ---------------------------------------------------------------------------

(def methods
  (rpc/registry
   {"math/add" (fn [{:keys [a b]} _] (+ a b))
    "echo/transport" (fn [_ {:keys [transport user]}] {:transport transport :user user})
    "slow" (fn [{:keys [ms]} _] (p/delay ms {:slept ms}))
    "fail" (fn [_ _] (throw (rpc/error :invalid-params "a and b are required")))
    "log" (fn [params _] (swap! results identity) (println "   (server) notification received:" (pr-str params)) nil)
    "jobs/run" (fn [{:keys [steps]} {:keys [notify! id]}]
                 (p/loop [i 1]
                   (if (> i steps)
                     {:done steps}
                     (p/let [_ (when notify! (notify! "jobs/progress" {:id id :step i}))]
                       (p/recur (inc i))))))}
   {:introspection? true}))

(defn build-app [static-root]
  (core/app {:env :test :cookie-secret "smoke-secret" :json true}
            {:use [(mw/request-id)]
             :get {"/" (fn [_] {:body "hello"})
                   "/json" (fn [{:keys [request-id]}] {:body {:ok true :request-id request-id}})
                   "/users/:id" (fn [{:keys [params query]}] {:body {:id (:id params) :q (:q query)}})
                   "/cookie" (fn [_] {:cookies [{:name "plain" :value "1"} {:name "sid" :value "abc" :signed? true :http-only? true}] :body "set"})
                   "/cookie/read" (fn [{:keys [cookies signed-cookies]}] {:body {:plain (:plain cookies) :sid (:sid signed-cookies)}})
                   "/redirect" (fn [_] {:redirect "/"})
                   "/stream" (fn [_] {:body (core/readable-from ["chunk-1|" "chunk-2|" "chunk-3"]) :type "text/plain"})
                   "/boom" (fn [_] (throw (core/http-error 418 "short and stout")))}
             :post {"/echo" [(mw/wrap-json (fn [{:keys [body]}] {:status 201 :body {:got body}}))]}
             :resource {"/thing" {:get (fn [_] {:body "thing"})}}
             :routers [{:mount "/static" :routes (route/routes (route/static "/" static-root))}]}
            (rpc/http-routes methods {:context (fn [req] {:user (get-in req [:headers :x-user])})})
            (rpc/http-routes methods {:path "/rpc-status" :status-map? true})))

(defn base [server] (tu/base-url server))

;; ---------------------------------------------------------------------------
;; Sections
;; ---------------------------------------------------------------------------

(defn http-round-trips! [server]
  (section! "1 HTTP round-trips")
  (let [b (base server)]
    (p/let [r1 (tu/fetch! (str b "/"))
            r2 (tu/fetch! (str b "/json"))
            r3 (tu/fetch! (str b "/users/42?q=x"))
            r4 (tu/fetch! (str b "/echo") {:method :post :body {:a [1 2] :camelKey "kept"}})
            r5 (tu/fetch! (str b "/cookie"))
            cookie-header (str/join "; " (map #(first (str/split % #";")) (:set-cookies r5)))
            r6 (tu/fetch! (str b "/cookie/read") {:headers {:cookie cookie-header}})
            r7 (tu/fetch! (str b "/redirect"))
            r8 (tu/fetch! (str b "/static/index.txt"))
            r9 (tu/fetch! (str b "/nope"))
            r10 (tu/fetch! (str b "/thing") {:method :post})
            r11 (tu/fetch! (str b "/boom"))
            r12 (tu/fetch! (str b "/echo") {:method :post :headers {:content-type "text/plain"} :body "x"})]
      (expect! "GET / text" ["hello" 200] [(:body r1) (:status r1)])
      (check! (and (= true (get-in r2 [:json :ok])) (string? (get-in r2 [:json :request-id]))
                   (= (get-in r2 [:json :request-id]) (get-in r2 [:headers :x-request-id])))
              "GET /json with request-id middleware" r2)
      (expect! "route params + query" {:id "42" :q "x"} (:json r3))
      (expect! "POST json body keys as spelled" [201 {:got {:a [1 2] :camelKey "kept"}}] [(:status r4) (:json r4)])
      (expect! "two Set-Cookie headers" 2 (count (:set-cookies r5)))
      (check! (some #(str/starts-with? % "sid=s%3A") (:set-cookies r5)) "signed cookie has the s: prefix" (:set-cookies r5))
      (expect! "cookies read back (signed verified)" {:plain "1" :sid "abc"} (:json r6))
      (expect! "redirect 302 + Location" [302 "/"] [(:status r7) (get-in r7 [:headers :location])])
      (expect! "static file through a mounted router" [200 "static ok\n"] [(:status r8) (:body r8)])
      (expect! "404 rendered as JSON by the wrapper" [404 404] [(:status r9) (get-in r9 [:json :error :status])])
      (expect! "405 with Allow from route/resource" [405 "GET, HEAD"] [(:status r10) (get-in r10 [:headers :allow])])
      (expect! "thrown http-error 418 exposed" [418 "short and stout"] [(:status r11) (get-in r11 [:json :error :message])])
      (expect! "wrap-json refuses a non-JSON body with 415" 415 (:status r12)))))

(defn streaming! [server]
  (section! "2 streaming :body")
  (p/let [r (tu/fetch! (str (base server) "/stream"))]
    (expect! "Readable body piped in order" "chunk-1|chunk-2|chunk-3" (:body r))
    (check! (str/starts-with? (str (get-in r [:headers :content-type])) "text/plain") "content type from :type" (:headers r))))

(defn graceful-close-with-keepalive! []
  (section! "3 close! with a keep-alive client connected")
  (p/let [server (core/listen! (core/app {:env :test} {:get {"/" (fn [_] {:body "ka"})}}) {:port 0 :host "127.0.0.1"})
          r (tu/fetch! (base server))
          n (core/connections! server)
          t0 (tu/now-ms)
          result (core/close! server {:timeout-ms 3000})
          elapsed (- (tu/now-ms) t0)
          _ (tu/sleep 20)]
    (expect! "request served" "ka" (:body r))
    (check! (<= 0 n 1) "at most one keep-alive connection open" n)
    (expect! "close! resolved without forcing" {:closed true :forced? false} result)
    (check! (< elapsed 1000) "idle keep-alive connection did not delay close (Node >= 19 reaps it)" elapsed)
    (check! (not (core/listening? server)) "server not listening")
    (check! (nil? (core/address server)) "address gone (the main smoke server is the one handle still listening)")))

(defn websockets! [server echo-wsv limited-wsv]
  (section! "4 websockets")
  (ws/on-open! echo-wsv (fn [conn]
                          (async/go-loop []
                            (when-let [m (async/<! (ws/messages conn))]
                              (if (= :binary (:type m))
                                (ws/send-binary! conn (:data m))
                                (ws/send! conn (str "echo:" (:data m))))
                              (recur)))))
  (p/let [c1 (ws/connect! (tu/ws-url server "/echo"))
          c2 (ws/connect! (tu/ws-url server "/echo"))
          c3 (ws/connect! (tu/ws-url server "/echo"))
          _ (tu/sleep 20)
          _ (ws/send! c1 "hi")
          m1 (tu/take-with-timeout (ws/messages c1))
          _ (ws/send-binary! c1 (tu/buffer-of "bytes!"))
          m2 (tu/take-with-timeout (ws/messages c1))
          [s1 s2 s3] (ws/conns echo-wsv)
          _ (ws/join! echo-wsv s1 "room")
          _ (ws/join! echo-wsv s2 "room")
          bc (ws/broadcast! echo-wsv {:all true})
          all (p/all (map #(tu/take-with-timeout (ws/messages %)) [c1 c2 c3]))
          em (ws/emit! echo-wsv "room" "room only")
          room (p/all (map #(tu/take-with-timeout (ws/messages %)) [c1 c2]))
          not-in-room (tu/take-with-timeout (ws/messages c3) 80)
          rtt (ws/ping! c1)
          rtt-s (ws/ping! s3)
          close-4001 (ws/close! s2 4001 "server says bye")
          client-saw (ws/closed c2)
          close-3000 (ws/close! c3 3000 "client says bye")
          server-saw (ws/closed s3)
          lim (ws/connect! (tu/ws-url server "/limited"))
          _ (ws/send! lim (tu/string-of-bytes 200))
          lim-close (ws/closed lim)
          _ (tu/sleep 20)]
    (expect! "text echo" "echo:hi" (:data m1))
    (expect! "binary frame round-trip" [:binary "bytes!"] [(:type m2) (tu/buffer->string (:data m2))])
    (expect! "broadcast! to all" {:sent 3 :failed 0 :errors []} bc)
    (check! (every? #(= {:all true} (ws/json %)) all) "all three received the JSON broadcast" all)
    (expect! "emit! to a topic" 2 (:sent em))
    (check! (every? #(= "room only" (:data %)) room) "topic members received" room)
    (expect! "non-member got nothing" ::tu/timeout not-in-room)
    (check! (and (number? rtt) (number? rtt-s)) "ping! round trips (client and server side)" [rtt rtt-s])
    (expect! "server close 4001 seen by client" {:code 4001 :reason "server says bye" :forced? false} client-saw)
    (expect! "client close 3000 seen by server" {:code 3000 :reason "client says bye" :forced? false} (select-keys server-saw [:code :reason :forced?]))
    (expect! "UNCERTAINTY 10.2: client sees 1009 after a maxPayload violation" 1009 (:code lim-close))
    (expect! "topics cleaned after close" #{"room"} (ws/topics echo-wsv))
    (expect! "registry count" [1 0] [(ws/conn-count echo-wsv) (ws/conn-count limited-wsv)])
    (ws/close! c1)))

(defn rpc-http! [server]
  (section! "5 RPC over HTTP")
  (let [b (base server)
        post (fn [path body & [opts]]
               (tu/fetch! (str b path) (merge {:method :post :headers {:content-type "application/json"}
                                              :body (if (string? body) body (ws/->json body))}
                                             opts)))]
    (p/let [r1 (post "/rpc" {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 2 :b 40}})
            r2 (post "/rpc" {:jsonrpc "2.0" :id 2 :method "fail"})
            r3 (post "/rpc" "{oops")
            r4 (post "/rpc" {:jsonrpc "2.0" :id 4 :method "nope"})
            r5 (post "/rpc" [{:jsonrpc "2.0" :id 5 :method "math/add"}])
            r6 (post "/rpc-status" {:jsonrpc "2.0" :id 6 :method "nope"})
            r7 (post "/rpc-status" {:jsonrpc "2.0" :id 7 :method "fail"})
            r8 (post "/rpc" {:jsonrpc "2.0" :method "log" :params {:from "http"}})
            r9 (tu/fetch! (str b "/rpc"))
            r10 (post "/rpc" {:jsonrpc "2.0" :id 10 :method "rpc.methods"})]
      (expect! "result frame, HTTP 200" [200 {:jsonrpc "2.0" :id 1 :result 42}] [(:status r1) (:json r1)])
      (expect! "method error: HTTP 200 + error frame" [200 -32602] [(:status r2) (get-in r2 [:json :error :code])])
      (expect! "malformed JSON: HTTP 400 + -32700 frame (documented decision)" [400 -32700 nil] [(:status r3) (get-in r3 [:json :error :code]) (get-in r3 [:json :id])])
      (expect! "unknown method -32601 (HTTP 200)" [200 -32601] [(:status r4) (get-in r4 [:json :error :code])])
      (expect! "batch rejected -32600 with :batch? marker" [400 -32600 true] [(:status r5) (get-in r5 [:json :error :code]) (get-in r5 [:json :error :data :batch?])])
      (expect! ":status-map? opt-in: -32601 -> 404" 404 (:status r6))
      (expect! ":status-map? opt-in: -32602 -> 400" 400 (:status r7))
      (expect! "notification -> 204" 204 (:status r8))
      (expect! "GET on the endpoint -> 405" 405 (:status r9))
      (check! (some #(= "math/add" (:name %)) (get-in r10 [:json :result])) "rpc.methods introspection" (:json r10)))))

(defn rpc-ws! [server rpc-wsv]
  (section! "6 RPC over websockets")
  (p/let [c (ws/connect! (tu/ws-url server "/rpc-ws") {:headers {:x-auth "smoke-user"}})
          client (rpc/client c)
          order (atom [])
          ;; vector binding: both calls in flight together (pitfall 9.28)
          [slow fast] (let [slow ((:call! client) "slow" {:ms 60})
                            fast ((:call! client) "slow" {:ms 5})]
                        (p/then slow (fn [_] (swap! order conj :slow)))
                        (p/then fast (fn [_] (swap! order conj :fast)))
                        [slow fast])
          results (p/all [slow fast])
          _ ((:notify! client) "log" {:from "ws"})
          job ((:call! client) "jobs/run" {:steps 2})
          n1 (tu/take-with-timeout (:notifications client))
          n2 (tu/take-with-timeout (:notifications client))
          err (tu/rejection ((:call! client) "fail"))
          _ (tu/sleep 10)
          [s] (ws/conns rpc-wsv)
          _ (rpc/notify! s "server/push" {:hello "client"})
          n3 (tu/take-with-timeout (:notifications client))
          _ (rpc/broadcast-notify! rpc-wsv "server/all" [1])
          n4 (tu/take-with-timeout (:notifications client))]
    (expect! "two concurrent calls correlated" [{:slept 60} {:slept 5}] results)
    (expect! "answers arrived as they finished" [:fast :slow] @order)
    (expect! "server->client notifications from a method (streaming primitive)" [{:id 3 :step 1} {:id 3 :step 2}] [(:params n1) (:params n2)])
    (expect! "then the result" {:done 2} job)
    (expect! "method error -> rejected call with the keyword code" :invalid-params (:code (ex-data err)))
    (expect! "notify! from the server" {:jsonrpc "2.0" :method "server/push" :params {:hello "client"}} n3)
    (expect! "broadcast-notify!" [1] (:params n4))
    ((:close! client))))

(defn same-registry! [server]
  (section! "7 one registry, two transports")
  (p/let [c (ws/connect! (tu/ws-url server "/rpc-ws") {:headers {:x-auth "ann"}})
          client (rpc/client c)
          via-ws ((:call! client) "echo/transport")
          via-http (tu/fetch! (str (base server) "/rpc") {:method :post :headers {:x-user "bob"}
                                                           :body {:jsonrpc "2.0" :id 1 :method "echo/transport"}})
          add-ws ((:call! client) "math/add" {:a 1 :b 1})
          add-http (tu/fetch! (str (base server) "/rpc") {:method :post :body {:jsonrpc "2.0" :id 2 :method "math/add" :params {:a 1 :b 1}}})]
    (expect! "ws call sees :transport :ws + the authenticate ctx" {:transport "ws" :user "ann"} via-ws)
    (expect! "http call sees :transport :http + the :context fn" {:transport "http" :user "bob"} (get-in via-http [:json :result]))
    (expect! "same method, same answer" [2 2] [add-ws (get-in add-http [:json :result])])
    ((:close! client))))

(defn authenticate! [server]
  (section! "8 authenticate")
  (p/let [e (tu/rejection (ws/connect! (tu/ws-url server "/rpc-ws")))
          c (ws/connect! (tu/ws-url server "/rpc-ws") {:headers {:x-auth "ok"}})
          e404 (tu/rejection (ws/connect! (tu/ws-url server "/no-such-endpoint")))]
    (expect! "no header -> HTTP 401 before the handshake" [:unexpected-response 401] [(:code (ex-data e)) (:status (ex-data e))])
    (check! (ws/open? c) "with header -> accepted")
    (expect! "unknown upgrade path -> 404" 404 (:status (ex-data e404)))
    (ws/close! c)))

(defn shutdown! [server rpc-wsv]
  (section! "9 full shutdown")
  (p/let [c (ws/connect! (tu/ws-url server "/rpc-ws") {:headers {:x-auth "x"}})
          client (rpc/client c {:timeout-ms 5000})
          [pending] [((:call! client) "slow" {:ms 2000})]
          port (:port (core/address server))
          t0 (tu/now-ms)
          result (core/close! server {:timeout-ms 3000 :ws-close-timeout-ms 500})
          elapsed (- (tu/now-ms) t0)
          closed (ws/closed c)
          pending-err (tu/rejection pending)
          _ (tu/sleep 30)
          again (core/listen! (core/app {:env :test} {:get {"/" (fn [_] {:body "again"})}}) {:port port :host "127.0.0.1"})
          _ (core/close! again)
          _ (tu/sleep 30)]
    (expect! "close! resolved with rpc call pending" {:closed true :forced? false} result)
    (check! (< elapsed 1500) "did not wait for the slow method" elapsed)
    (expect! "ws client saw 1001 server shutdown" {:code 1001 :reason "server shutdown" :forced? false} closed)
    (expect! "pending call rejected on close" :closed (:code (ex-data pending-err)))
    (expect! "rpc ws registry empty" 0 (ws/conn-count rpc-wsv))
    (check! (not (core/listening? server)) "server closed")
    (check! (some? (:address again)) "port freed: could listen on it again" port)
    (check! (not (some #{"TCPServerWrap"} (tu/active-resources))) "no server handle left" (tu/active-resources))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn main [& _]
  (let [t0 (tu/now-ms)
        static-root (tu/temp-dir {"index.txt" "static ok\n"})]
    (tu/on-exit! (fn [code]
                   (println (str "\n[liveness] process exited by itself after " (- (tu/now-ms) t0)
                                 " ms with code " code "; active resources at exit: " (pr-str (tu/active-resources))))))
    (println "express-cljs smoke — real 127.0.0.1 server, HTTP + ws + RPC")
    (section! "0 scaffold")
    (-> (p/let [server (core/listen! (build-app static-root) {:port 0 :host "127.0.0.1"})
                echo-wsv (ws/serve! server {:path "/echo" :keepalive-ms 1000})
                limited-wsv (ws/serve! server {:path "/limited" :max-payload 64 :keepalive-ms false})
                rpc-wsv (ws/serve! server {:path "/rpc-ws" :keepalive-ms 1000
                                           :authenticate (fn [{:keys [headers]}] (when-let [u (:x-auth headers)] {:user u}))})
                _ (rpc/serve-ws! rpc-wsv methods)]
          (check! (core/listening? server) "server listening" (core/address server))
          (check! (= 3 (count (map :path [echo-wsv limited-wsv rpc-wsv]))) "three ws endpoints on one server")
          (p/do (http-round-trips! server)
                (streaming! server)
                (graceful-close-with-keepalive!)
                (websockets! server echo-wsv limited-wsv)
                (rpc-http! server)
                (rpc-ws! server rpc-wsv)
                (same-registry! server)
                (authenticate! server)
                (shutdown! server rpc-wsv)))
        (p/catch (fn [e]
                   (check! false (str "unexpected failure: " (ex-message e)) (ex-data e))))
        (p/finally (fn [_ _]
                     (let [rs @results
                           failed (remove :ok? rs)]
                       (println (str "\n== tally: " (count rs) " checks, " (- (count rs) (count failed)) " passed, " (count failed) " failed"))
                       (doseq [f failed] (println (str "   FAIL " (:n f) " [" (:section f) "] " (:label f))))
                       (println (if (empty? failed) "SMOKE PASS" "SMOKE FAIL"))
                       (println "(no process.exit: the process now has to drain by itself — unref'd keepalive timers must not hold it)")
                       (tu/set-exit-code! (if (empty? failed) 0 1))))))))
