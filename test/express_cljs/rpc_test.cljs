(ns express-cljs.rpc-test
  "The RPC layer: pure frame / registry / dispatch tests (no server), then
   the HTTP transport (Node fetch against a loopback server) and the
   websocket transport (`ws/connect!` + `rpc/client`), including one test
   that calls the same registry over both transports."
  (:refer-clojure :exclude [methods])
  (:require [cljs.core.async :as async]
            [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.rpc :as rpc]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [express-cljs.test-util :as tu]))

(def methods
  (rpc/registry
   {"math/add" (fn [{:keys [a b]} _] (+ a b))
    "math/sum" (fn [xs _] (reduce + xs))
    "echo/ctx" (fn [_ ctx] (-> ctx (dissoc :conn :notify! :request) (assoc :has-conn? (some? (:conn ctx)) :has-request? (some? (:request ctx)))))
    "slow" {:fn (fn [{:keys [ms]} _] (p/delay (or ms 50) {:slept ms})) :doc "sleeps"}
    "slow/limited" {:fn (fn [_ _] (p/delay 300 :never)) :timeout-ms 30}
    "fail/rpc" (fn [_ _] (throw (rpc/error :invalid-params "a and b required" {:missing [:a]})))
    "fail/arg" (fn [_ _] (throw (util/argument-error "bad arg")))
    "fail/http401" (fn [_ _] (throw (core/http-error 401 "login first")))
    "fail/http403" (fn [_ _] (throw (core/http-error 403 "no")))
    "fail/http418" (fn [_ _] (throw (core/http-error 418 "tea")))
    "fail/bug" (fn [_ _] (throw (ex-info "secret detail" {:x 1})))
    "fail/reject" (fn [_ _] (p/rejected (rpc/error :forbidden "nope")))
    "nil/result" (fn [_ _] nil)
    "notify/me" (fn [{:keys [n]} {:keys [notify! id]}]
                  (p/let [_ (when notify! (notify! "progress" {:for id :pct 50}))
                          _ (when notify! (notify! "progress" {:for id :pct 100}))]
                    {:done n}))
    "auth/whoami" (fn [_ {:keys [user]}]
                    (when-not user (throw (rpc/error :unauthorized "who are you?")))
                    {:user user})}
   {:introspection? true}))

(defn- app* [& sources] (apply core/app {:env :test} sources))

(defn- rpc-post
  "POST a frame (CLJS map -> JSON) to /rpc; `raw` posts a string verbatim."
  ([base frame] (rpc-post base frame {}))
  ([base frame {:keys [raw headers path] :or {path "/rpc"}}]
   (tu/fetch! (str base path)
              {:method :post
               :headers (merge {:content-type "application/json"} headers)
               :body (if raw frame (ws/->json frame))})))

;; ---------------------------------------------------------------------------
;; Pure: frames
;; ---------------------------------------------------------------------------

(deftest frame-constructors
  (is (= {:jsonrpc "2.0" :id 1 :method "m" :params {:a 1}} (rpc/request-frame 1 "m" {:a 1})))
  (is (= {:jsonrpc "2.0" :id "x" :method "m"} (rpc/request-frame "x" "m" nil)) "params omitted when nil")
  (is (= {:jsonrpc "2.0" :method "n" :params [1]} (rpc/notification-frame "n" [1])))
  (is (= {:jsonrpc "2.0" :id 1 :result nil} (rpc/response-frame 1 nil)) "result member always present")
  (is (= {:jsonrpc "2.0" :id nil :error {:code -32700 :message "Parse error"}} (rpc/error-frame nil :parse-error "Parse error")))
  (is (= {:jsonrpc "2.0" :id 2 :error {:code -32602 :message "bad" :data {:k 1}}} (rpc/error-frame 2 :invalid-params "bad" {:k 1})))
  (is (= -32001 (get-in (rpc/error-frame 1 :unauthorized "x") [:error :code])))
  (is (= 1234 (get-in (rpc/error-frame 1 1234 "raw code") [:error :code])) "raw integers pass through")
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/error-frame 1 :nope "x")))))
  (is (rpc/request? {:method "m"}))
  (is (rpc/notification? {:jsonrpc "2.0" :method "m"}))
  (is (not (rpc/notification? {:jsonrpc "2.0" :method "m" :id nil})) "id null is a request, not a notification")
  (is (rpc/response? {:result 1}))
  (is (rpc/error-response? {:error {:code 1}}))
  (is (not (rpc/error-response? {:result 1})))
  (is (= 7 (rpc/frame-id {:id 7})))
  (is (nil? (rpc/frame-id {:id {:bad 1}})) "undetectable id -> nil")
  (is (nil? (rpc/frame-id [1 2]))))

(deftest check-frame-validation
  (is (= {:method "m" :params {:a 1} :id 1 :notification? false}
         (rpc/check-frame {:jsonrpc "2.0" :id 1 :method "m" :params {:a 1}})))
  (is (= {:method "m" :params nil :id nil :notification? true}
         (rpc/check-frame {:jsonrpc "2.0" :method "m"})))
  (is (= [1 2] (:params (rpc/check-frame {:jsonrpc "2.0" :id "s" :method "m" :params [1 2]}))) "positional params")
  (letfn [(invalid [frame]
            (let [e (tu/thrown #(rpc/check-frame frame))]
              (is (= :invalid-request (util/rpc-error-code e)) (pr-str frame))
              (ex-data e)))]
    (let [d (invalid [{:jsonrpc "2.0" :id 1 :method "m"} {:jsonrpc "2.0" :id 2 :method "m"}])]
      (is (true? (:batch? d)) "batch marker (uncertainty 10.13)")
      (is (= 2 (:count d)))
      (is (nil? (:id d))))
    (is (nil? (:id (invalid "string"))))
    (is (nil? (:id (invalid 42))))
    (is (= 1 (:id (invalid {:jsonrpc "1.0" :id 1 :method "m"}))) "a detectable id is echoed")
    (is (= "jsonrpc must be \"2.0\"" (:reason (invalid {:id 1 :method "m"}))))
    (is (= "method must be a non-empty string" (:reason (invalid {:jsonrpc "2.0" :id 1}))))
    (is (= "method must be a non-empty string" (:reason (invalid {:jsonrpc "2.0" :id 1 :method ""}))))
    (is (= "params must be an object or an array" (:reason (invalid {:jsonrpc "2.0" :id 1 :method "m" :params "x"}))))
    (is (= "id must be a string, a number or null" (:reason (invalid {:jsonrpc "2.0" :id {:o 1} :method "m"}))))
    (is (nil? (:id (invalid {:jsonrpc "2.0" :id {:o 1} :method "m"}))) "an invalid id is not echoed")))

(deftest error-to-frame-mapping
  (let [f #(rpc/error->frame 9 % {})]
    (is (= {:jsonrpc "2.0" :id 9 :error {:code -32602 :message "a and b required" :data {:missing [:a]}}}
           (f (rpc/error :invalid-params "a and b required" {:data {:missing [:a]}}))))
    (is (= -32601 (get-in (f (rpc/error :method-not-found "x")) [:error :code])))
    (is (= -32602 (get-in (f (util/argument-error "bad arg")) [:error :code])) "argument-error -> invalid params")
    (is (= "express-cljs: bad arg" (get-in (f (util/argument-error "bad arg")) [:error :message])))
    (is (= {:code -32001 :message "login first"} (:error (f (core/http-error 401 "login first")))))
    (is (= {:code -32002 :message "no"} (:error (f (core/http-error 403 "no")))))
    (is (= {:code -32000 :message "tea" :data {:status 418}} (:error (f (core/http-error 418 "tea")))))
    (is (= "Service Unavailable" (get-in (f (core/http-error 503 "db down")) [:error :message])) "5xx messages hidden")
    (is (= {:code -32603 :message "Internal error"} (:error (f (ex-info "secret" {})))) "foreign errors hidden by default")
    (is (= {:code -32603 :message "secret"} (:error (rpc/error->frame 9 (ex-info "secret" {}) {:expose-errors? true}))))
    (is (= {:code -32603 :message "Internal error"} (:error (f (tu/js-error "js boom")))))
    (p/let [e (tu/rejection (p/timeout (p/deferred) 5))]
      (is (= {:code -32003 :message "method timed out"} (:error (f e)))))))

(deftest frame-to-error-inverse
  (let [e (rpc/frame->error {:jsonrpc "2.0" :id 3 :error {:code -32601 :message "method not found: x" :data {:method "x"}}})]
    (is (= :express-cljs/rpc-error (util/error-type e)))
    (is (= :method-not-found (:code (ex-data e))))
    (is (= 3 (:id (ex-data e))))
    (is (= {:method "x"} (:data (ex-data e))))
    (is (= "express-cljs rpc method-not-found: method not found: x" (ex-message e))))
  (let [e (rpc/frame->error {:id 1 :error {:code -31000 :message "custom"}})]
    (is (= :application (:code (ex-data e))))
    (is (= -31000 (:raw-code (ex-data e))))))

;; ---------------------------------------------------------------------------
;; Pure: registry
;; ---------------------------------------------------------------------------

(deftest registry-validation-and-builtins
  (is (rpc/registry? methods))
  (is (not (rpc/registry? {:methods {}})))
  (is (fn? (:fn (rpc/method methods "math/add"))))
  (is (= "sleeps" (:doc (rpc/method methods "slow"))))
  (is (= 30 (:timeout-ms (rpc/method methods "slow/limited"))))
  (is (nil? (rpc/method methods "nope")))
  (is (fn? (:fn (rpc/method methods "rpc.ping"))) "rpc.ping built in")
  (is (fn? (:fn (rpc/method methods "rpc.methods"))) "introspection opt-in")
  (is (nil? (rpc/method (rpc/registry {"a" (fn [_ _] 1)}) "rpc.methods")) "introspection off by default")
  (is (nil? (rpc/method (rpc/registry {"a" (fn [_ _] 1)} {:ping? false}) "rpc.ping")))
  (is (= [{:name "a" :doc "A"} {:name "b" :doc nil}] (rpc/methods (rpc/registry {"b" (fn [_ _] 1) "a" {:fn (fn [_ _] 1) :doc "A"}} {:ping? false})))
      "sorted, docs included")
  (doseq [bad [{"" (fn [_ _] 1)} {:kw (fn [_ _] 1)} {"rpc.custom" (fn [_ _] 1)} {"x" 42} {"x" {:doc "no fn"}} {"x" {:fn (fn [_ _] 1) :bogus 1}} "not a map"]]
    (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/registry bad)))) (pr-str bad)))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/registry {} {:bogus true}))))))

;; ---------------------------------------------------------------------------
;; Pure: dispatch
;; ---------------------------------------------------------------------------

(deftest call-results-and-errors
  (async done
    (tu/finish done
      (let [ctx {:transport :test}
            call (fn [frame & [opts]] (rpc/call! methods frame ctx (or opts {})))]
        (p/let [r1 (call {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 1 :b 2}})
                r2 (call {:jsonrpc "2.0" :id "s" :method "math/sum" :params [1 2 3]})
                r3 (call {:jsonrpc "2.0" :id 3 :method "nope"})
                r4 (call {:jsonrpc "2.0" :id 4 :method "fail/rpc"})
                r5 (call {:jsonrpc "2.0" :id 5 :method "fail/arg"})
                r6 (call {:jsonrpc "2.0" :id 6 :method "fail/bug"})
                r6x (call {:jsonrpc "2.0" :id 6 :method "fail/bug"} {:expose-errors? true})
                r7 (call {:jsonrpc "2.0" :id 7 :method "fail/reject"})
                r8 (call {:jsonrpc "2.0" :id 8 :method "nil/result"})
                r9 (call {:jsonrpc "2.0" :id 9 :method "slow" :params {:ms 10}})
                r10 (call {:jsonrpc "2.0" :id 10 :method "slow/limited"})
                r11 (call {:jsonrpc "2.0" :id 11 :method "slow" :params {:ms 100}} {:timeout-ms 20})
                r12 (call {:jsonrpc "2.0" :id nil :method "math/add" :params {:a 1 :b 1}})
                r13 (call {:jsonrpc "2.0" :id 13 :method "rpc.ping"})
                r14 (call {:jsonrpc "2.0" :id 14 :method "rpc.methods"})
                r15 (call {:jsonrpc "2.0" :id 15 :method "echo/ctx" :params {:p 1}})]
          (is (= {:jsonrpc "2.0" :id 1 :result 3} r1))
          (is (= {:jsonrpc "2.0" :id "s" :result 6} r2) "string ids echoed, positional params")
          (is (= {:jsonrpc "2.0" :id 3 :error {:code -32601 :message "method not found: nope" :data {:method "nope"}}} r3) "-32601")
          (is (= {:code -32602 :message "a and b required" :data {:missing [:a]}} (:error r4)))
          (is (= -32602 (get-in r5 [:error :code])))
          (is (= {:code -32603 :message "Internal error"} (:error r6)) "bugs hidden")
          (is (= "secret detail" (get-in r6x [:error :message])) ":expose-errors? shows the message")
          (is (= -32002 (get-in r7 [:error :code])) "rejected promises")
          (is (= {:jsonrpc "2.0" :id 8 :result nil} r8))
          (is (= {:slept 10} (:result r9)) "promise results")
          (is (= -32003 (get-in r10 [:error :code])) "per-method :timeout-ms")
          (is (= -32003 (get-in r11 [:error :code])) "dispatch-level :timeout-ms")
          (is (= {:jsonrpc "2.0" :id nil :result 2} r12) "id null is answered")
          (is (= {:pong true} (:result r13)))
          (is (some #(= "math/add" (:name %)) (:result r14)))
          (is (= {:transport :test :id 15 :method "echo/ctx" :notification? false :params {:p 1} :has-conn? false :has-request? false}
                 (:result r15)) "ctx injection"))))))

(deftest call-notifications-and-invalid-frames
  (async done
    (tu/finish done
      (let [errors (atom [])
            opts {:on-error (fn [e ctx] (swap! errors conj [(util/rpc-error-code e) (:id ctx)]))}
            call (fn [frame] (rpc/call! methods frame {} opts))]
        (p/let [n1 (call {:jsonrpc "2.0" :method "math/add" :params {:a 1 :b 2}})
                n2 (call {:jsonrpc "2.0" :method "nope"})
                n3 (call {:jsonrpc "2.0" :method "fail/rpc"})
                b (call [{:jsonrpc "2.0" :id 1 :method "math/add"}])
                i1 (call {:foo 1})
                i2 (call "text")
                i3 (call {:jsonrpc "2.0" :id 5 :method 42})]
          (is (nil? n1) "a notification is never answered")
          (is (nil? n2) "not even for an unknown method (spec 4.1)")
          (is (nil? n3))
          (is (= [[:method-not-found nil] [:invalid-params nil]] (take 2 @errors)) ":on-error sees the notification failures")
          (is (= [:invalid-request :invalid-request :invalid-request :invalid-request] (map first (drop 2 @errors))) "and every error frame produced")
          (is (= {:jsonrpc "2.0" :id nil :error {:code -32600 :message "Invalid Request: batch not supported"
                                                 :data {:batch? true :count 1 :reason "batch not supported"}}}
                 (update-in b [:error :data] dissoc :id)) "batch -> -32600 with the marker")
          (is (= -32600 (get-in i1 [:error :code])) "an invalid frame without id is answered with id null")
          (is (nil? (:id i1)))
          (is (= -32600 (get-in i2 [:error :code])))
          (is (= 5 (:id i3)) "a detectable id is echoed on an invalid frame"))))))

(deftest call-option-validation
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/http-routes methods {:batch? true})))) "batch is a reserved marker")
  (is (map? (rpc/http-routes methods {:batch? false})))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/http-routes methods {:bogus 1})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/http-routes {:methods {}} {})))) "not a registry")
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/serve-ws! {} {:methods {}} {})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(rpc/serve-ws! {} methods {:bogus 1}))))))

(deftest error-status-mapping
  (let [nf (rpc/error-frame 1 :method-not-found "x")
        un (rpc/error-frame 1 :unauthorized "x")
        raw (rpc/error-frame 1 -31000 "x")]
    (is (= 200 (rpc/error-status nf nil)))
    (is (= 200 (rpc/error-status nf false)))
    (is (= 404 (rpc/error-status nf true)))
    (is (= 401 (rpc/error-status un true)))
    (is (= 500 (rpc/error-status raw true)) "unknown codes -> 500")
    (is (= 422 (rpc/error-status nf {:method-not-found 422})) "a map overrides")
    (is (= 401 (rpc/error-status un {:method-not-found 422})) "and falls back to the table")))

;; ---------------------------------------------------------------------------
;; HTTP transport
;; ---------------------------------------------------------------------------

(deftest http-results-and-method-errors-answer-200
  (async done
    (tu/finish done
      (tu/with-server (app* (rpc/http-routes methods {:context (fn [req] {:user (get-in req [:headers :x-user])})}))
        (fn [_ base]
          (p/let [r1 (rpc-post base {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 2 :b 3}})
                  r2 (rpc-post base {:jsonrpc "2.0" :id 2 :method "nope"})
                  r3 (rpc-post base {:jsonrpc "2.0" :id 3 :method "fail/rpc"})
                  r4 (rpc-post base {:jsonrpc "2.0" :id 4 :method "auth/whoami"})
                  r5 (rpc-post base {:jsonrpc "2.0" :id 5 :method "auth/whoami"} {:headers {:x-user "ann"}})
                  r6 (rpc-post base {:jsonrpc "2.0" :id 6 :method "echo/ctx"})
                  r7 (rpc-post base {:jsonrpc "2.0" :id 7 :method "notify/me" :params {:n 1}})]
            (is (= 200 (:status r1)))
            (is (str/includes? (get-in r1 [:headers :content-type]) "application/json"))
            (is (= {:jsonrpc "2.0" :id 1 :result 5} (:json r1)))
            (is (= 200 (:status r2)) "method errors are HTTP 200 (inventory 7.2)")
            (is (= -32601 (get-in r2 [:json :error :code])))
            (is (= 200 (:status r3)))
            (is (= {:code -32602 :message "a and b required" :data {:missing ["a"]}} (get-in r3 [:json :error])))
            (is (= -32001 (get-in r4 [:json :error :code])))
            (is (= {:user "ann"} (get-in r5 [:json :result])) ":context injected from the request")
            (is (= {:transport "http" :id 6 :method "echo/ctx" :notification? false :params nil :has-conn? false :has-request? true :user nil}
                   (get-in r6 [:json :result])))
            (is (= {:done 1} (get-in r7 [:json :result])) "notify! is nil over HTTP; the method still completes")))))))

(deftest http-status-map-opt-in
  (async done
    (tu/finish done
      (tu/with-server (app* (rpc/http-routes methods {:status-map? true :path "/api/rpc"}))
        (fn [_ base]
          (p/let [r1 (rpc-post base {:jsonrpc "2.0" :id 1 :method "nope"} {:path "/api/rpc"})
                  r2 (rpc-post base {:jsonrpc "2.0" :id 2 :method "auth/whoami"} {:path "/api/rpc"})
                  r3 (rpc-post base {:jsonrpc "2.0" :id 3 :method "fail/bug"} {:path "/api/rpc"})
                  r4 (rpc-post base {:jsonrpc "2.0" :id 4 :method "math/add" :params {:a 1 :b 1}} {:path "/api/rpc"})
                  r5 (rpc-post base {:jsonrpc "2.0" :id 5 :method "fail/http418"} {:path "/api/rpc"})]
            (is (= 404 (:status r1)))
            (is (= -32601 (get-in r1 [:json :error :code])) "the body stays a JSON-RPC error object")
            (is (= 401 (:status r2)))
            (is (= 500 (:status r3)))
            (is (= 200 (:status r4)))
            (is (= 500 (:status r5)) ":application maps to 500; the original status travels in data")
            (is (= 418 (get-in r5 [:json :error :data :status])))))
        )
      )))

(deftest http-status-map-as-a-map
  (async done
    (tu/finish done
      (tu/with-server (app* (rpc/http-routes methods {:status-map {:method-not-found 422}}))
        (fn [_ base]
          (p/let [r1 (rpc-post base {:jsonrpc "2.0" :id 1 :method "nope"})
                  r2 (rpc-post base {:jsonrpc "2.0" :id 2 :method "fail/rpc"})]
            (is (= 422 (:status r1)))
            (is (= 400 (:status r2)) "codes not in the map use the table")))))))

(deftest http-transport-failures
  (async done
    (tu/finish done
      (tu/with-server (app* (rpc/http-routes methods {:limit "1kb"}))
        (fn [_ base]
          (p/let [bad-json (rpc-post base "{not json" {:raw true})
                  batch (rpc-post base [{:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 1 :b 1}}])
                  scalar (rpc-post base "42" {:raw true})
                  no-ct (tu/fetch! (str base "/rpc") {:method :post :headers {:content-type "text/plain"} :body "{}"})
                  empty (tu/fetch! (str base "/rpc") {:method :post :headers {:content-type "application/json"}})
                  get-req (tu/fetch! (str base "/rpc"))
                  big (rpc-post base (ws/->json {:jsonrpc "2.0" :id 1 :method "math/sum" :params (vec (range 2000))}) {:raw true})
                  invalid (rpc-post base {:jsonrpc "2.0" :id 9 :method 42})
                  other (tu/fetch! (str base "/other"))]
            (is (= 400 (:status bad-json)) "malformed JSON: 400 + -32700 (documented decision)")
            (is (= {:jsonrpc "2.0" :id nil :error {:code -32700 :message "Parse error" :data {:reason "malformed JSON body"}}} (:json bad-json)))
            (is (= 400 (:status batch)))
            (is (= -32600 (get-in batch [:json :error :code])))
            (is (true? (get-in batch [:json :error :data :batch?])))
            (is (= 400 (:status scalar)) "a JSON scalar is an invalid request, not a parse error (non-strict parser)")
            (is (= -32600 (get-in scalar [:json :error :code])))
            (is (= 415 (:status no-ct)))
            (is (= -32600 (get-in no-ct [:json :error :code])))
            (is (= 400 (:status empty)))
            (is (= -32600 (get-in empty [:json :error :code])) "fetch sends content-length 0: body-parser yields {} -> invalid request")
            (is (string? (get-in empty [:json :error :data :reason])))
            (is (= 405 (:status get-req)) "non-POST -> 405 through route/resource")
            (is (= "POST" (get-in get-req [:headers :allow])))
            (is (= 413 (:status big)))
            (is (= -32600 (get-in big [:json :error :code])))
            (is (= "body too large" (get-in big [:json :error :data :reason])))
            (is (= 400 (:status invalid)) "invalid request frames are transport failures")
            (is (= 9 (get-in invalid [:json :id])))
            (is (= 404 (:status other)) "the error entry only covers the rpc path")))))))

(deftest http-notification-answers-204
  (async done
    (tu/finish done
      (let [seen (atom [])
            reg (rpc/registry {"log" (fn [params _] (swap! seen conj params) nil)})]
        (tu/with-server (app* (rpc/http-routes reg))
          (fn [_ base]
            (p/let [r (rpc-post base {:jsonrpc "2.0" :method "log" :params {:msg "hi"}})
                    r2 (rpc-post base {:jsonrpc "2.0" :method "nope"})]
              (is (= 204 (:status r)))
              (is (= "" (:body r)))
              (is (= [{:msg "hi"}] @seen))
              (is (= 204 (:status r2)) "an unknown notification is silently dropped (spec)"))))))))

(deftest http-context-failure-answers-at-http-level
  (async done
    (tu/finish done
      (tu/with-server (app* (rpc/http-routes methods {:context (fn [req]
                                                                  (if (get-in req [:headers :x-user])
                                                                    (p/resolved {:user "p"})
                                                                    (throw (core/http-error 401 "token required"))))}))
        (fn [_ base]
          (p/let [r1 (rpc-post base {:jsonrpc "2.0" :id 1 :method "auth/whoami"})
                  r2 (rpc-post base {:jsonrpc "2.0" :id 2 :method "auth/whoami"} {:headers {:x-user "x"}})]
            (is (= 401 (:status r1)) "a thrown http-error in :context is a wrapper error response")
            (is (= "token required" (get-in r1 [:json :error :message])))
            (is (= {:user "p"} (get-in r2 [:json :result])) "a promise context is awaited")))))))

;; ---------------------------------------------------------------------------
;; Websocket transport
;; ---------------------------------------------------------------------------

(defn- with-rpc-ws
  "Fixture: app with http-routes + ws endpoint + serve-ws!; f receives
   {:server :wsv :base :rpc-ws}."
  [serve-opts ws-opts f]
  (tu/with-server (app* (rpc/http-routes methods))
    (fn [server base]
      (let [wsv (ws/serve! server (merge {:path "/ws" :keepalive-ms false} ws-opts))
            rw (rpc/serve-ws! wsv methods serve-opts)]
        (f {:server server :wsv wsv :base base :rpc-ws rw})))))

(deftest ws-request-response-correlation-with-concurrent-calls
  (async done
    (tu/finish done
      (with-rpc-ws {} {}
        (fn [{:keys [server]}]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  client (rpc/client c)
                  order (atom [])
                  ;; vector binding: the calls must stay in flight together (pitfall 9.28)
                  [slow fast add] (let [slow ((:call! client) "slow" {:ms 80})
                                        fast ((:call! client) "slow" {:ms 5})
                                        add ((:call! client) "math/add" {:a 20 :b 22})]
                                    (p/then slow (fn [_] (swap! order conj :slow)))
                                    (p/then fast (fn [_] (swap! order conj :fast)))
                                    (p/then add (fn [_] (swap! order conj :add)))
                                    (is (= 3 ((:pending client))))
                                    [slow fast add])
                  results (p/all [slow fast add])
                  ping ((:call! client) "rpc.ping")]
            (is (= [{:slept 80} {:slept 5} 42] results) "each answer reaches its own call")
            (is (= [:add :fast :slow] @order) "answers arrive as methods finish, not in request order")
            (is (= 0 ((:pending client))))
            (is (= {:pong true} ping))
            ((:close! client))))))))

(deftest ws-errors-and-malformed-frames
  (async done
    (tu/finish done
      (with-rpc-ws {} {}
        (fn [{:keys [server]}]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  _ (ws/send! c "not json at all")
                  parse-err (tu/take-with-timeout (ws/messages c))
                  _ (ws/send-binary! c (tu/buffer-of "bin"))
                  bin-err (tu/take-with-timeout (ws/messages c))
                  _ (ws/send-json! c [{:jsonrpc "2.0" :id 1 :method "rpc.ping"}])
                  batch-err (tu/take-with-timeout (ws/messages c))
                  client (rpc/client c)
                  e1 (tu/rejection ((:call! client) "nope"))
                  e2 (tu/rejection ((:call! client) "fail/rpc"))
                  e3 (tu/rejection ((:call! client) "fail/http401"))
                  e4 (tu/rejection ((:call! client) "slow/limited"))
                  ok ((:call! client) "math/add" {:a 1 :b 1})]
            (is (= {:jsonrpc "2.0" :id nil :error {:code -32700 :message "Parse error" :data {:reason "express-cljs ws: invalid JSON: Unexpected token 'o', \"not json at all\" is not valid JSON"}}}
                   (ws/json parse-err)) "-32700 with id null for a non-JSON text frame")
            (is (= -32600 (get-in (ws/json bin-err) [:error :code])) "binary frames are not JSON-RPC")
            (is (= -32600 (get-in (ws/json batch-err) [:error :code])))
            (is (true? (get-in (ws/json batch-err) [:error :data :batch?])))
            (is (= :method-not-found (:code (ex-data e1))))
            (is (= 1 (:id (ex-data e1))) "the client's ids are counters")
            (is (= :invalid-params (:code (ex-data e2))))
            (is (= {:missing ["a"]} (:data (ex-data e2))))
            (is (= :unauthorized (:code (ex-data e3))))
            (is (= :timeout (:code (ex-data e4))) "server-side method timeout -> -32003")
            (is (= 2 ok) "the connection survives every error")
            ((:close! client))))))))

(deftest ws-notifications-both-ways
  (async done
    (tu/finish done
      (let [seen (atom [])
            reg (rpc/registry {"log" (fn [params {:keys [transport]}] (swap! seen conj [transport params]) nil)
                               "notify/me" (rpc/method methods "notify/me")})]
        (tu/with-server (app*)
          (fn [server _]
            (let [wsv (ws/serve! server {:path "/ws" :keepalive-ms false})
                  _ (rpc/serve-ws! wsv reg)]
              (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                      client (rpc/client c)
                      _ ((:notify! client) "log" {:m 1})
                      _ ((:notify! client) "log")
                      _ ((:notify! client) "nope")
                      _ (tu/sleep 30)
                      r ((:call! client) "notify/me" {:n 7})
                      n1 (tu/take-with-timeout (:notifications client))
                      n2 (tu/take-with-timeout (:notifications client))
                      _ (tu/sleep 10)
                      [s] (ws/conns wsv)
                      _ (rpc/notify! s "server/push" {:x 1})
                      n3 (tu/take-with-timeout (:notifications client))
                      b (rpc/broadcast-notify! wsv "server/all" [1 2])
                      n4 (tu/take-with-timeout (:notifications client))
                      silence (tu/take-with-timeout (:notifications client) 40)]
                (is (= [[:ws {:m 1}] [:ws nil]] @seen) "client notifications reach the handler, never answered")
                (is (= {:done 7} r))
                (is (= {:jsonrpc "2.0" :method "progress" :params {:for 1 :pct 50}} n1) "notify! from the method's ctx, before the result")
                (is (= {:for 1 :pct 100} (:params n2)))
                (is (= {:jsonrpc "2.0" :method "server/push" :params {:x 1}} n3))
                (is (= {:sent 1 :failed 0 :errors []} b))
                (is (= {:jsonrpc "2.0" :method "server/all" :params [1 2]} n4))
                (is (= ::tu/timeout silence) "an unknown notification produced no frame")
                ((:close! client))))))))))

(deftest ws-context-injection-from-authenticate-and-context
  (async done
    (tu/finish done
      (with-rpc-ws {:context (fn [conn] {:extra (count (:id conn)) :user (str (:user (ws/ctx conn)) "!")})}
                   {:authenticate (fn [{:keys [headers]}] (when-let [u (:x-user headers)] {:user u :role "admin"}))}
        (fn [{:keys [server wsv]}]
          (p/let [e (tu/rejection (ws/connect! (tu/ws-url server "/ws")))
                  c (ws/connect! (tu/ws-url server "/ws") {:headers {:x-user "ann"}})
                  client (rpc/client c)
                  r ((:call! client) "echo/ctx" {:q 1})
                  who ((:call! client) "auth/whoami")]
            (is (= 401 (:status (ex-data e))) "authenticate gates the RPC socket")
            (is (= {:transport :ws :id 1 :method "echo/ctx" :notification? false :params {:q 1}
                    :user "ann!" :role "admin" :extra 36 :has-conn? true :has-request? false}
                   (-> r (update :transport keyword)))
                "connection ctx, then :context, then the transport keys")
            (is (= {:user "ann!"} who))
            (is (= (:id (first (ws/conns wsv))) (:id (first (ws/conns wsv)))))
            ((:close! client))))))))

(deftest same-registry-over-both-transports
  (async done
    (tu/finish done
      (with-rpc-ws {} {}
        (fn [{:keys [server base]}]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  client (rpc/client c)
                  via-ws ((:call! client) "math/add" {:a 40 :b 2})
                  via-http (rpc-post base {:jsonrpc "2.0" :id "h" :method "math/add" :params {:a 40 :b 2}})
                  ctx-ws ((:call! client) "echo/ctx")
                  ctx-http (rpc-post base {:jsonrpc "2.0" :id "h2" :method "echo/ctx"})
                  nf-ws (tu/rejection ((:call! client) "nope"))
                  nf-http (rpc-post base {:jsonrpc "2.0" :id "h3" :method "nope"})]
            (is (= 42 via-ws))
            (is (= 42 (get-in via-http [:json :result])))
            (is (= "ws" (:transport ctx-ws)))
            (is (= "http" (get-in ctx-http [:json :result :transport])))
            (is (= -32601 (get-in nf-http [:json :error :code])))
            (is (= :method-not-found (:code (ex-data nf-ws))) "one error mapping")
            ((:close! client))))))))

(deftest ws-strict-mode-closes-3001
  (async done
    (tu/finish done
      (with-rpc-ws {:strict? true} {}
        (fn [{:keys [server]}]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  _ (ws/send! c "garbage")
                  r (ws/closed c)]
            (is (= 3001 (:code r)) "reserved RPC close code (inventory 7.4)")
            (is (= "protocol violation: non-JSON frame" (:reason r)))))))))

(deftest ws-serve-hooks-and-stop
  (async done
    (tu/finish done
      (let [opened (atom 0) closed (atom [])]
        (with-rpc-ws {:on-open (fn [_] (swap! opened inc)) :on-close (fn [_ info] (swap! closed conj (:code info)))} {}
          (fn [{:keys [server rpc-ws]}]
            (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                    client (rpc/client c)
                    r ((:call! client) "rpc.ping")
                    _ ((:close! client))
                    _ (tu/sleep 20)
                    _ ((:stop! rpc-ws))
                    c2 (ws/connect! (tu/ws-url server "/ws"))
                    _ (ws/send-json! c2 (rpc/request-frame 1 "rpc.ping" nil))
                    silence (tu/take-with-timeout (ws/messages c2) 60)]
              (is (= {:pong true} r))
              (is (= 1 @opened))
              (is (= [1000] @closed))
              (is (= ::tu/timeout silence) "after stop! new connections are not served")
              (ws/close! c2))))))))

(deftest ws-client-timeout-and-close-rejects-pending
  (async done
    (tu/finish done
      (with-rpc-ws {} {}
        (fn [{:keys [server]}]
          (p/let [c (ws/connect! (tu/ws-url server "/ws"))
                  client (rpc/client c {:timeout-ms 30 :id-prefix "req-"})
                  e1 (tu/rejection ((:call! client) "slow" {:ms 200}))
                  c2 (ws/connect! (tu/ws-url server "/ws"))
                  client2 (rpc/client c2)
                  ;; plain let: the call must still be in flight when the socket closes (pitfall 9.28)
                  [pending] [((:call! client2) "slow" {:ms 500})]
                  _ (tu/sleep 10)
                  _ (ws/close! c2)
                  e2 (tu/rejection pending)]
            (is (= :timeout (:code (ex-data e1))) "client-side deadline")
            (is (= "req-1" (:id (ex-data e1))))
            (is (= :closed (:code (ex-data e2))) "closing the connection rejects every pending call")
            (is (= 0 ((:pending client2))))
            (ws/close! c)))))))

(deftest shutdown-with-rpc-calls-pending
  (async done
    (tu/finish done
      (p/let [server (core/listen! (app* (rpc/http-routes methods)) {:port 0 :host "127.0.0.1"})
              wsv (ws/serve! server {:path "/ws" :keepalive-ms false})
              _ (rpc/serve-ws! wsv methods)
              c (ws/connect! (tu/ws-url server "/ws"))
              client (rpc/client c {:timeout-ms 2000})
              [pending] [((:call! client) "slow" {:ms 400})]
              _ (tu/sleep 10)
              t0 (system-time)
              r (core/close! server {:timeout-ms 1000 :ws-close-timeout-ms 300})
              elapsed (- (system-time) t0)
              cr (ws/closed c)
              e (tu/rejection pending)
              _ (tu/sleep 20)]
        (is (= {:closed true :forced? false} r))
        (is (< elapsed 300) (str "close! did not wait for the slow method: " elapsed "ms"))
        (is (= 1001 (:code cr)))
        (is (= :closed (:code (ex-data e))) "the client's pending call rejected on close")
        (is (not (core/listening? server)))
        (is (not (some #{"TCPServerWrap"} (tu/active-resources))))))))
