(ns express-cljs.ws-test
  "The websocket binding: pure helpers plus real loopback round-trips (an
   ephemeral 127.0.0.1 port per test, `ws/connect!` as the client, closed
   by the fixture). Interop-free: every JS touch is in test-util or in the
   two binding namespaces."
  (:require [cljs.core.async :as async]
            [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [express-cljs.test-util :as tu]))

(defn- app* [] (core/app {:env :test} {:get {"/" (fn [_] {:body "http ok"})}}))

(defn- with-ws
  "Fixture: app + listen + serve! with `opts`; calls `(f wsv server)`;
   closes the server (and therefore the websocket endpoint) afterwards."
  [opts f]
  (tu/with-server (app*) (fn [server _]
                           (let [wsv (ws/serve! server opts)]
                             (f wsv server)))))

(defn- echo-server!
  "Every text / binary frame goes straight back."
  [wsv]
  (ws/on-open! wsv (fn [conn]
                     (async/go-loop []
                       (when-let [m (async/<! (ws/messages conn))]
                         (ws/send! conn (:data m))
                         (recur))))))

;; ---------------------------------------------------------------------------
;; Pure
;; ---------------------------------------------------------------------------

(deftest json-helpers-keep-keys-as-spelled
  (is (= "{\"maxAge\":1,\"k\":[1,\"x\"]}" (ws/->json {:maxAge 1 :k [1 :x]})))
  (is (= {:maxAge 1 :nested {:a-b [1 2]}} (ws/parse-json "{\"maxAge\":1,\"nested\":{\"a-b\":[1,2]}}")))
  (is (= {:x 1} (ws/parse-json {:type :text :data "{\"x\":1}"})) "a message map works too")
  (let [e (tu/thrown #(ws/parse-json "{nope"))]
    (is (= :express-cljs/ws-error (util/error-type e)))
    (is (= :invalid-json (:code (ex-data e))))))

(deftest close-code-validation
  (is (ws/valid-close-code? 1000))
  (is (ws/valid-close-code? 1001))
  (is (ws/valid-close-code? 3000))
  (is (ws/valid-close-code? 4999))
  (doseq [bad [999 1004 1005 1006 1015 2999 5000 "1000" nil]]
    (is (not (ws/valid-close-code? bad)) (str bad))))

(deftest ws-error-mapping-from-js-errors
  (let [e (ws/->ws-error (tu/js-error-with-code "Max payload size exceeded" "WS_ERR_UNSUPPORTED_MESSAGE_LENGTH") {:id "c1"})]
    (is (= :express-cljs/ws-error (util/error-type e)))
    (is (= :unsupported-message-length (:code (ex-data e))))
    (is (= 1009 (:close-code (ex-data e))) "what the peer receives (inventory 6.6)")
    (is (= "c1" (:conn-id (ex-data e)))))
  (is (= 1007 (:close-code (ex-data (ws/->ws-error (tu/js-error-with-code "bad utf8" "WS_ERR_INVALID_UTF8") nil)))))
  (is (= 1002 (:close-code (ex-data (ws/->ws-error (tu/js-error-with-code "rsv" "WS_ERR_UNEXPECTED_RSV_1") nil)))))
  (let [e (ws/->ws-error (tu/js-error "socket hang up") nil)]
    (is (= :socket (:code (ex-data e))))
    (is (nil? (:close-code (ex-data e)))))
  (let [own (util/ws-error "mine" {:code :closed})]
    (is (identical? own (ws/->ws-error own nil)) "express-cljs errors pass through")))

(deftest serve-option-validation
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (let [e (tu/thrown #(ws/serve! server {:bogus 1}))]
            (is (= :express-cljs/argument-error (util/error-type e)))
            (is (= #{:bogus} (:unknown (ex-data e)))))
          (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(ws/serve! server {:path 42})))))
          (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(ws/serve! {} {})))) "not a server handle")
          (let [wsv (ws/serve! server {:path "/ws" :keepalive-ms false})]
            (is (= "/ws" (:path wsv)))
            (is (= 0 (ws/conn-count wsv)))
            (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(ws/serve! server {:path "/ws"}))))
                "same path twice on one server")
            (is (map? (ws/serve! server {:path "/other"})) "a second endpoint on another path is fine")))))))

;; ---------------------------------------------------------------------------
;; Round-trips
;; ---------------------------------------------------------------------------

(deftest echo-text-and-binary
  (async done
    (tu/finish done
      (with-ws {:path "/echo" :keepalive-ms false}
        (fn [wsv server]
          (echo-server! wsv)
          (p/let [c (ws/connect! (tu/ws-url server "/echo"))
                  _ (is (true? (:client? c)))
                  _ (is (ws/open? c))
                  _ (is (= :open (ws/ready-state c)))
                  r1 (ws/send! c "hello")
                  m1 (tu/take-with-timeout (ws/messages c))
                  _ (ws/send-binary! c (tu/buffer-of "raw bytes"))
                  m2 (tu/take-with-timeout (ws/messages c))
                  _ (ws/send! c {:text "enveloped"})
                  m3 (tu/take-with-timeout (ws/messages c))
                  _ (ws/send-json! c {:a 1 :camelKey [1 2]})
                  m4 (tu/take-with-timeout (ws/messages c))
                  _ (ws/send! c :kw)
                  m5 (tu/take-with-timeout (ws/messages c))]
            (is (= {:bytes 5} r1) "send! resolves with the byte count")
            (is (= {:type :text :data "hello"} m1))
            (is (= :binary (:type m2)))
            (is (= "raw bytes" (tu/buffer->string (:data m2))) "a Buffer round-trips as a binary frame (pitfall 9.21)")
            (is (= "enveloped" (:data m3)))
            (is (= {:a 1 :camelKey [1 2]} (ws/json m4)) "JSON keys as spelled")
            (is (= "kw" (:data m5)) "keywords send their name")
            (is (= 1 (ws/conn-count wsv)))
            (is (zero? (ws/buffered-amount c)))
            (p/let [r (ws/close! c 1000 "done")
                    _ (tu/sleep 20)]
              (is (= {:code 1000 :reason "done" :forced? false} r) "client-initiated close resolves with the close event")
              (is (= 0 (ws/conn-count wsv)) "closed connection left the registry"))))))))

(deftest decode-json-option
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :decode :json :keepalive-ms false}
        (fn [wsv server]
          (let [seen (async/chan 8)]
            (ws/on-open! wsv (fn [conn]
                               (async/go-loop []
                                 (when-let [m (async/<! (ws/messages conn))]
                                   (async/>! seen m)
                                   (recur)))))
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    _ (ws/send! c "{\"userId\":7}")
                    m1 (tu/take-with-timeout seen)
                    _ (ws/send! c "not json")
                    m2 (tu/take-with-timeout seen)
                    _ (ws/send-binary! c (tu/buffer-of "bin"))
                    m3 (tu/take-with-timeout seen)]
              (is (= {:userId 7} (:json m1)))
              (is (= "not json" (:data m2)))
              (is (string? (:json-error m2)) "unparsable text carries :json-error, not an exception")
              (is (not (contains? m2 :json)))
              (is (= :binary (:type m3)))
              (ws/close! c))))))))

(deftest out-channel-sends-in-order
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (ws/on-open! wsv (fn [conn]
                             (async/go
                               (async/>! (ws/out conn) "one")
                               (async/>! (ws/out conn) {:json {:n 2}})
                               (async/>! (ws/out conn) (tu/buffer-of "three")))))
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  ms (tu/collect (ws/messages c) 3)]
            (is (= "one" (:data (first ms))))
            (is (= {:n 2} (ws/json (second ms))))
            (is (= :binary (:type (nth ms 2))))
            (is (= "three" (tu/buffer->string (:data (nth ms 2)))))
            (ws/close! c)))))))

(deftest connection-map-fields-and-upgrade-map
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false :protocols ["rpc" "chat"]
                :authenticate (fn [{:keys [headers query cookies]}]
                                {:user (:x-user headers) :q (:q query) :sid (:sid cookies)})}
        (fn [wsv server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws?q=1&z=2")
                                 {:headers {:x-user "ann" :cookie "sid=abc; other=1"} :protocols ["chat" "rpc"]})
                  _ (tu/sleep 20)]
            (let [[conn] (ws/conns wsv)]
              (is (string? (:id conn)))
              (is (= {:user "ann" :q "1" :sid "abc"} (ws/ctx conn)) "authenticate result is the ctx")
              (is (= "127.0.0.1" (:remote-addr conn)))
              (is (number? (:remote-port conn)))
              (is (= "127.0.0.1" (:ip conn)) "Express's req.ip through the prototype trick (10.9)")
              (is (= "ann" (get-in conn [:headers :x-user])) "lower-case kebab headers")
              (is (= "websocket" (get-in conn [:headers :upgrade])))
              (is (= :ws (:protocol conn)))
              (is (= "chat" (:subprotocol conn)) "first client-offered protocol the server accepts")
              (is (= "chat" (:subprotocol c)) "the client agrees")
              (let [u (ws/upgrade conn)]
                (is (= :ws (:transport u)))
                (is (= :get (:method u)))
                (is (= "/ws?q=1&z=2" (:url u)))
                (is (= "/ws" (:path u)))
                (is (= {:q "1" :z "2"} (:query u)))
                (is (= {:sid "abc" :other "1"} (:cookies u)))
                (is (= "127.0.0.1" (:ip u)))
                (is (= :http (:protocol u)))
                (is (nil? (:body u)))
                (is (= {} (:params u))))
              (is (= conn (ws/conn-by-id wsv (:id conn))))
              (is (number? (:opened-at conn)))
              (is (map? (:handshake-headers c)))
              (is (= "websocket" (get-in c [:handshake-headers :upgrade])))
              (is (= "chat" (get-in c [:handshake-headers :sec-websocket-protocol]))))
            (ws/close! c)))))))

(deftest handshake-headers-option
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false
                :handshake-headers (fn [{:keys [query]}] {:set-cookie (str "ws=" (:t query)) :x-hello "world"})}
        (fn [_ server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws?t=9"))]
            (is (= "world" (get-in c [:handshake-headers :x-hello])))
            (is (= ["ws=9"] (get-in c [:handshake-headers :set-cookie])) "set-cookie is always a vector (pitfall 9.24)")
            (ws/close! c)))))))

(deftest connections-channel-open-and-close-once
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (let [events (ws/connections wsv)
                closes (atom 0)]
            (ws/on-close! wsv (fn [_ _] (swap! closes inc)))
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    open (tu/take-with-timeout events)
                    _ (is (= :open (:event open)))
                    _ (is (= (:id (first (ws/conns wsv))) (:id (:conn open))))
                    _ (ws/close! c 4001 "app says bye")
                    close (tu/take-with-timeout events)
                    _ (tu/sleep 30)
                    tail (tu/take-with-timeout events 50)]
              (is (= :close (:event close)))
              (is (= 4001 (:code close)) "application close codes round-trip (inventory 6.6)")
              (is (= "app says bye" (:reason close)))
              (is (false? (:forced? close)))
              (is (= (:id (:conn open)) (:id (:conn close))))
              (is (= 1 @closes) "exactly one close per connection (uncertainty 10.17)")
              (is (= ::tu/timeout tail) "no further events"))))))))

(deftest registry-and-topics
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (p/let [c1 (ws/connect! (tu/ws-url server "/ws"))
                  c2 (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)]
            (let [[a b] (ws/conns wsv)]
              (is (= 2 (ws/conn-count wsv)))
              (is (= 1 (ws/join! wsv a "room:1")))
              (is (= 2 (ws/join! wsv b "room:1")))
              (is (= 1 (ws/join! wsv b :room-2)) "keyword topics")
              (is (= #{"room:1"} (ws/topics wsv a)))
              (is (= #{"room:1" "room-2"} (ws/topics wsv b)))
              (is (= #{"room:1" "room-2"} (ws/topics wsv)))
              (is (= #{(:id a) (:id b)} (set (map :id (ws/topic->conns wsv "room:1")))))
              (is (= 1 (ws/leave! wsv a "room:1")))
              (is (= #{} (ws/topics wsv a)))
              (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(ws/join! wsv a 42)))))
              (p/let [_ (ws/close! c2)
                      _ (tu/sleep 30)]
                (is (= 1 (ws/conn-count wsv)))
                (is (= #{} (ws/topics wsv)) "a closing connection leaves every topic")
                (is (= [] (ws/topic->conns wsv "room:1")))
                (is (= 0 (ws/join! wsv b "room:1")) "joining after close is a no-op")
                (ws/close! c1)))))))))

(deftest broadcast-and-emit
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (p/let [c1 (ws/connect! (tu/ws-url server "/ws"))
                  c2 (ws/connect! (tu/ws-url server "/ws"))
                  c3 (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)]
            (let [[a b c] (ws/conns wsv)]
              (ws/join! wsv a "t")
              (ws/join! wsv b "t")
              (p/let [r1 (ws/broadcast! wsv {:hello "all"})
                      got (p/all (map #(tu/take-with-timeout (ws/messages %)) [c1 c2 c3]))
                      r2 (ws/emit! wsv "t" "topic msg")
                      got-t (p/all (map #(tu/take-with-timeout (ws/messages %)) [c1 c2]))
                      none (tu/take-with-timeout (ws/messages c3) 60)
                      r3 (ws/broadcast! wsv "t" "not to a" {:except a})
                      got-b (tu/take-with-timeout (ws/messages c2))
                      none-a (tu/take-with-timeout (ws/messages c1) 60)
                      r4 (ws/broadcast! wsv [c] "explicit list")
                      got-c (tu/take-with-timeout (ws/messages c3))]
                (is (= {:sent 3 :failed 0 :errors []} r1))
                (is (every? #(= {:hello "all"} (ws/json %)) got) "plain maps go out as JSON")
                (is (= {:sent 2 :failed 0 :errors []} r2))
                (is (every? #(= "topic msg" (:data %)) got-t))
                (is (= ::tu/timeout none) "a connection outside the topic gets nothing")
                (is (= 1 (:sent r3)))
                (is (= "not to a" (:data got-b)))
                (is (= ::tu/timeout none-a) ":except skipped the sender")
                (is (= 1 (:sent r4)))
                (is (= "explicit list" (:data got-c)))
                (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(ws/broadcast! wsv 42 "x")))))
                (p/all (map ws/close! [c1 c2 c3]))))))))))

;; ---------------------------------------------------------------------------
;; Handshake: authenticate, 404, protocols
;; ---------------------------------------------------------------------------

(deftest authenticate-rejects-with-real-http-statuses
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false
                :authenticate (fn [{:keys [headers]}]
                                (case (:x-auth headers)
                                  "ok" (p/resolved {:user "u"})
                                  "forbidden" (throw (core/http-error 403 "no entry"))
                                  "boom" (throw (ex-info "bug" {}))
                                  nil))}
        (fn [wsv server]
          (p/let [e1 (tu/rejection (ws/connect! (tu/ws-url server "/ws")))
                  e2 (tu/rejection (ws/connect! (tu/ws-url server "/ws") {:headers {:x-auth "forbidden"}}))
                  e3 (tu/rejection (ws/connect! (tu/ws-url server "/ws") {:headers {:x-auth "boom"}}))
                  c (ws/connect! (tu/ws-url server "/ws") {:headers {:x-auth "ok"}})
                  _ (tu/sleep 10)]
            (is (= :express-cljs/ws-error (util/error-type e1)))
            (is (= :unexpected-response (:code (ex-data e1))))
            (is (= 401 (:status (ex-data e1))) "nil ctx -> 401 before the handshake (inventory 2.3)")
            (is (= 403 (:status (ex-data e2))) "a thrown http-error answers its status")
            (is (= 500 (:status (ex-data e3))) "any other throw -> 500")
            (is (ws/open? c))
            (is (= {:user "u"} (ws/ctx (first (ws/conns wsv)))) "a promise ctx is awaited")
            (is (= 1 (ws/conn-count wsv)) "rejected handshakes never registered")
            (let [errs (atom [])]
              (async/go-loop []
                (when-let [ev (async/<! (ws/connections wsv))]
                  (when (= :error (:event ev)) (swap! errs conj ev))
                  (recur)))
              (p/let [_ (tu/sleep 20)]
                (is (>= (count @errs) 2) "auth failures are reported on the connections channel")
                (is (every? #(nil? (:conn %)) @errs))
                (is (some #(= "/ws" (get-in % [:upgrade :path])) @errs))))
            (ws/close! c)))))))

(deftest unknown-upgrade-path-gets-404
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [_ server]
          (p/let [e (tu/rejection (ws/connect! (tu/ws-url server "/nope")))
                  http (tu/fetch! (str (tu/base-url server) "/"))]
            (is (= :unexpected-response (:code (ex-data e))))
            (is (= 404 (:status (ex-data e))) "the wrapper answers non-matching upgrades itself (inventory 2.3)")
            (is (= 200 (:status http)) "plain HTTP keeps working on the same server")))))))

(deftest two-endpoints-and-path-forms
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (let [a (ws/serve! server {:path "/a" :keepalive-ms false})
                b (ws/serve! server {:path #{"/b" "/b2"} :keepalive-ms false})
                c (ws/serve! server {:path #(str/starts-with? % "/pred/") :keepalive-ms false})]
            (p/let [ca (ws/connect! (tu/ws-url server "/a"))
                    cb (ws/connect! (tu/ws-url server "/b2"))
                    cc (ws/connect! (tu/ws-url server "/pred/x"))
                    e (tu/rejection (ws/connect! (tu/ws-url server "/pred")))
                    _ (tu/sleep 20)]
              (is (= 1 (ws/conn-count a)))
              (is (= 1 (ws/conn-count b)))
              (is (= 1 (ws/conn-count c)))
              (is (= 404 (:status (ex-data e))))
              (is (= "/b2" (get-in (first (ws/conns b)) [:upgrade :path])))
              (p/all (map ws/close! [ca cb cc])))))))))

(deftest protocol-negotiation-failure
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false :protocols ["rpc"]}
        (fn [_ server]
          (p/let [e (tu/rejection (ws/connect! (tu/ws-url server "/ws") {:protocols ["other"]}))
                  c (ws/connect! (tu/ws-url server "/ws"))]
            (is (= :express-cljs/ws-error (util/error-type e)) "client offered protocols, server accepted none (inventory 6.9)")
            (is (nil? (:subprotocol c)) "a client offering nothing is fine")
            (ws/close! c)))))))

(deftest protocols-as-a-function
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false
                :protocols (fn [offered {:keys [query]}] (when (= "yes" (:pick query)) (last offered)))}
        (fn [_ server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws?pick=yes") {:protocols ["a" "b"]})]
            (is (= "b" (:subprotocol c)) "the fn sees the offered vector and the upgrade map")
            (ws/close! c)))))))

;; ---------------------------------------------------------------------------
;; Close codes, errors, limits
;; ---------------------------------------------------------------------------

(deftest max-payload-violation-client-sees-1009
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :max-payload 64 :keepalive-ms false}
        (fn [wsv server]
          (let [events (ws/connections wsv)]
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    _ (tu/take-with-timeout events)
                    _ (ws/send! c (tu/string-of-bytes 200))
                    client-close (ws/closed c)
                    ev1 (tu/take-with-timeout events)
                    ev2 (tu/take-with-timeout events)]
              (is (= 1009 (:code client-close)) "UNCERTAINTY 10.2 SETTLED: the client sees 1009 Message Too Big")
              (is (= "" (:reason client-close)) "ws sends the code without a reason")
              (let [{err :error-ev cls :close-ev} (into {} (map (fn [ev] [(if (= :error (:event ev)) :error-ev :close-ev) ev]) [ev1 ev2]))]
                (is (some? err) "server side: an error event")
                (is (= :unsupported-message-length (:code (ex-data (:error err)))))
                (is (= 1009 (:close-code (ex-data (:error err)))))
                (is (some? cls) "then a close event")
                (is (= 1006 (:code cls)) "server-side close reports 1006 (receiver stopped before the peer's reply, inventory 6.2)")
                (is (= :unsupported-message-length (:code (ex-data (:error cls)))) "the close event carries the error")))))))))

(deftest close-codes-both-directions
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (p/let [c1 (ws/connect! (tu/ws-url server "/ws"))
                  c2 (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)]
            (let [[s1 s2] (ws/conns wsv)]
              (p/let [r1 (ws/close! s1 4001 "server bye")
                      client-side (ws/closed c1)
                      r2 (ws/close! c2 3000 "client bye")
                      server-side (ws/closed s2)
                      again (ws/close! c2)]
                (is (= 4001 (:code r1)))
                (is (= "server bye" (:reason r1)))
                (is (= {:code 4001 :reason "server bye" :forced? false} client-side))
                (is (= {:code 3000 :reason "client bye" :forced? false} r2))
                (is (= 3000 (:code server-side)))
                (is (= r2 again) "closing a closed connection resolves with the same event")
                (is (= :closed (ws/ready-state c2)))
                (let [e (tu/thrown #(ws/close! c2 1005))]
                  (is (= :invalid-close-code (:code (ex-data e))) "ws would throw; the wrapper says why (pitfall 9.22)"))
                (let [e (tu/thrown #(ws/close! c2 1000 (tu/string-of-bytes 124)))]
                  (is (= :invalid-close-reason (:code (ex-data e)))))
                (p/let [e (tu/rejection (ws/send! c2 "late"))]
                  (is (= :closed (:code (ex-data e))) "send after close rejects, never throws"))))))))))

(deftest terminate-gives-1006-on-both-sides
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)
                  s (first (ws/conns wsv))
                  r (ws/terminate! s)
                  cr (ws/closed c)]
            (is (= 1006 (:code r)))
            (is (true? (:forced? r)))
            (is (= 1006 (:code cr)))))))))

(deftest ping-pong-round-trip
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false}
        (fn [wsv server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)
                  s (first (ws/conns wsv))
                  ms1 (ws/ping! s)
                  ms2 (ws/ping! c "payload")
                  _ (ws/close! c)
                  e (tu/rejection (ws/ping! c))]
            (is (and (number? ms1) (>= ms1 0)) "server -> client ping answered by autoPong")
            (is (and (number? ms2) (>= ms2 0)) "client -> server ping answered by autoPong")
            (is (= :closed (:code (ex-data e))))))))))

(deftest keepalive-pings-from-an-unrefd-interval
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (let [wsv (ws/serve! server {:path "/ws" :keepalive-ms 25})]
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    _ (tu/sleep 120)
                    stats (ws/keepalive-stats wsv)
                    _ (is (>= (:pings stats) 2) (str "the interval pinged: " stats))
                    _ (is (= 0 (:terminated stats)) "a live peer (autoPong) is never terminated")
                    _ (ws/close! c)
                    _ (ws/stop! wsv)
                    _ (tu/sleep 80)]
              (is (= (:pings stats) (:pings (ws/keepalive-stats wsv))) "stop! cleared the interval")
              (is (not (some #(= "TCPSocketWrap" %) (tu/active-resources))) "no socket handle left"))))))))

(deftest keepalive-terminates-a-dead-peer
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (let [wsv (ws/serve! server {:path "/ws" :keepalive-ms 30})]
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    _ (tu/sleep 10)
                    s (first (ws/conns wsv))
                    _ (tu/pause-socket! c)
                    r (ws/closed s)]
              (is (= 1006 (:code r)) "no pong within one interval -> terminate() (README pattern, inventory 6.7)")
              (is (true? (:forced? r)))
              (is (= 1 (:terminated (ws/keepalive-stats wsv))))
              (ws/terminate! c))))))))

(deftest receive-backpressure-pauses-and-resumes
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false :message-buffer 1}
        (fn [wsv server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  _ (tu/sleep 20)
                  s (first (ws/conns wsv))
                  _ (p/all (map #(ws/send! c (str "m" %)) (range 10)))
                  _ (tu/sleep 30)
                  got (tu/collect (ws/messages s) 10)]
            (is (= (map #(str "m" %) (range 10)) (map :data got)) "all frames arrive, in order, through a 1-slot buffer")
            (ws/close! c)))))))

(deftest send-backpressure-limit
  (async done
    (tu/finish done
      (with-ws {:path "/ws" :keepalive-ms false :max-buffered-bytes 1024}
        (fn [wsv server]
          (p/let [c (ws/connect! (tu/ws-url server "/ws") {:max-payload 67108864})
                  _ (tu/sleep 20)
                  s (first (ws/conns wsv))
                  _ (tu/pause-socket! c)
                  big (tu/string-of-bytes 33554432)
                  ;; vector binding: the big send must stay in flight (pitfall 9.28)
                  [first-send] [(-> (ws/send! s big) (p/catch (fn [e] [:failed (:code (ex-data e))])))]
                  e (tu/rejection (ws/send! s "second"))]
            (is (= :backpressure (:code (ex-data e))) "a send while the socket buffer is over the limit rejects instead of queueing (inventory 6.8)")
            (is (> (:buffered-amount (ex-data e)) 1024))
            (is (> (ws/buffered-amount s) 1024))
            (p/let [r (ws/terminate! s)
                    _ (tu/resume-socket! c)
                    outcome (p/promise first-send)
                    _ (ws/closed c)]
              (is (= 1006 (:code r)))
              (is (= :ecanceled (second outcome)) "the interrupted write reports the socket error, no close code")
              (is (= 0 (ws/conn-count wsv))))))))))

;; ---------------------------------------------------------------------------
;; Shutdown choreography (inventory 5.3, uncertainty 10.17)
;; ---------------------------------------------------------------------------

(deftest close-choreography-with-connected-clients
  (async done
    (tu/finish done
      (p/let [server (core/listen! (app*) {:port 0 :host "127.0.0.1"})
              wsv (ws/serve! server {:path "/ws" :keepalive-ms 20})
              closes (atom [])
              _ (ws/on-close! wsv (fn [conn info] (swap! closes conj [(:id conn) info])))
              c1 (ws/connect! (tu/ws-url server "/ws"))
              c2 (ws/connect! (tu/ws-url server "/ws"))
              _ (tu/sleep 20)
              _ (is (= 2 (ws/conn-count wsv)))
              t0 (system-time)
              r (core/close! server {:timeout-ms 1000 :ws-close-timeout-ms 500})
              elapsed (- (system-time) t0)
              cr1 (ws/closed c1)
              cr2 (ws/closed c2)
              stop-result (ws/stop! wsv)
              tail (tu/drain-until-closed (ws/connections wsv) 50)
              _ (tu/sleep 20)]
        (is (= {:closed true :forced? false} r) "server.close resolved without forcing")
        (is (< elapsed 500) (str "no deadline waited on: " elapsed "ms"))
        (is (= {:code 1001 :reason "server shutdown" :forced? false} cr1) "clients see 1001 going away")
        (is (= {:code 1001 :reason "server shutdown" :forced? false} cr2))
        (is (= 0 (ws/conn-count wsv)))
        (is (= 2 (count @closes)) "one close per connection")
        (is (= {:closed 2 :terminated 0} stop-result) "the close hook's stop! result (idempotent second call)")
        (is (= ::tu/closed tail) "connections channel closed by stop!")
        (is (not (core/listening? server)))
        (is (not (some #(= "TCPServerWrap" %) (tu/active-resources))) "no server handle left")))))

(deftest close-terminates-an-unresponsive-client-once
  (async done
    (tu/finish done
      (p/let [server (core/listen! (app*) {:port 0 :host "127.0.0.1"})
              wsv (ws/serve! server {:path "/ws" :keepalive-ms false})
              closes (atom 0)
              _ (ws/on-close! wsv (fn [_ _] (swap! closes inc)))
              c1 (ws/connect! (tu/ws-url server "/ws"))
              c2 (ws/connect! (tu/ws-url server "/ws"))
              _ (tu/sleep 20)
              _ (tu/pause-socket! c1)
              t0 (system-time)
              r (core/close! server {:timeout-ms 2000 :ws-close-timeout-ms 150})
              elapsed (- (system-time) t0)
              stop-result (ws/stop! wsv)
              cr2 (ws/closed c2)
              _ (tu/sleep 50)]
        (is (= {:closed true :forced? false} r))
        (is (<= 140 elapsed 1500) (str "the ws deadline (150ms) fired, not the 2s HTTP one: " elapsed "ms"))
        (is (= {:closed 2 :terminated 1} stop-result) "wrapper terminate() won over ws closeTimeout (uncertainty 10.17)")
        (is (= 1001 (:code cr2)) "the responsive client closed cleanly")
        (is (= 2 @closes) "exactly one close event per connection, terminated one included")
        (is (not (core/listening? server)))
        (ws/terminate! c1)))))

(deftest stop-is-idempotent-and-refuses-new-upgrades
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (let [wsv (ws/serve! server {:path "/ws" :keepalive-ms false})]
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    r1 (ws/stop! wsv {:timeout-ms 200})
                    r2 (ws/stop! wsv)
                    cr (ws/closed c)
                    e (tu/rejection (ws/connect! (tu/ws-url server "/ws")))
                    http (tu/fetch! (str (tu/base-url server) "/"))]
              (is (= {:closed 1 :terminated 0} r1))
              (is (= r1 r2))
              (is (= 1001 (:code cr)))
              (is (= 404 (:status (ex-data e))) "the endpoint is detached: the shared upgrade listener answers 404")
              (is (= 200 (:status http)) "HTTP keeps running after a standalone stop!"))))))))
