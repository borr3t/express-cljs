(ns express-cljs.middleware-test
  "Middleware descriptors and Ring-style wrappers: pure tests over request
   maps, plus one loopback round-trip through core."
  (:require [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.middleware :as mw]
            [express-cljs.route :as route]
            [express-cljs.util :as util]
            [express-cljs.test-util :as tu]))

(def ^:private req {:method :get :original-url "/x" :path "/x" :headers {} :accepts ["*/*"]})

(deftest parser-descriptors-validate-options
  (is (= {:express-cljs/builtin :json :opts {}} (mw/json)))
  (is (= {:express-cljs/builtin :json :opts {:limit "1mb" :strict false}} (mw/json {:limit "1mb" :strict? false})))
  (is (= {:express-cljs/builtin :urlencoded :opts {:extended true :parameter-limit 10}} (mw/urlencoded {:extended? true :parameter-limit 10})))
  (is (= :text (:express-cljs/builtin (mw/text {:default-charset "utf-8"}))))
  (is (= :raw (:express-cljs/builtin (mw/raw {:type ["application/octet-stream" "image/*"]}))))
  (doseq [f [#(mw/json {:extended? true}) #(mw/raw {:strict? true}) #(mw/text {:bogus 1}) #(mw/urlencoded "x")]]
    (is (= :express-cljs/argument-error (util/error-type (tu/thrown f))))))

(deftest static-descriptor-validates
  (is (= {:express-cljs/builtin :static :root "/srv" :opts {:dotfiles :allow :max-age "1d" :index false}}
         (mw/static "/srv" {:dotfiles :allow :max-age "1d" :index false})))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(mw/static "/srv" {:dotfiles :maybe})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(mw/static "" {})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(mw/static "/srv" {:hidden true}))))))

(deftest request-id-reuses-or-generates
  (let [f (mw/request-id)
        given (f (assoc req :headers {:x-request-id "abc"}))
        fresh (f req)]
    (is (= (route/continue {:request-id "abc"} {:x-request-id "abc"}) given))
    (is (route/control? fresh))
    (is (= 36 (count (get-in fresh [:assoc :request-id]))) "a UUID when absent")
    (is (= (get-in fresh [:assoc :request-id]) (get-in fresh [:headers :x-request-id])))
    (is (= {:express-cljs/next :next :assoc {:request-id "q"} :headers {:x-rid "q"}}
           ((mw/request-id {:header :x-rid}) (assoc req :headers {:x-rid "q"}))))))

(deftest wrap-json-forces-json-and-guards-body-type
  (async done
    (tu/finish done
      (let [h (mw/wrap-json (fn [_] {:body "plain"}))
            passthrough (mw/wrap-json (fn [_] nil))]
        (p/let [r (h req)
                n (passthrough req)
                e (tu/rejection (h (assoc req :content-type "text/plain" :content-length 3)))
                ok (h (assoc req :content-type "application/json" :content-length 3))
                lenient ((mw/wrap-json (fn [_] {:body {:a 1}}) {:require-json-body? false})
                         (assoc req :content-type "text/plain" :content-length 3))]
          (is (= {:body "plain" :json true} r))
          (is (nil? n) "nil passes through")
          (is (= 415 (util/error-status e)))
          (is (= {:body "plain" :json true} ok))
          (is (= {:body {:a 1} :json true} lenient)))))))

(deftest wrap-timing-adds-header
  (async done
    (tu/finish done
      (p/let [r ((mw/wrap-timing (fn [_] (p/let [_ (tu/sleep 2)] {:body "x"}))) req)
              n ((mw/wrap-timing (fn [_] (route/next-route))) req)]
        (is (re-matches #"\d+(\.\d)?ms" (get-in r [:headers :x-response-time])))
        (is (= (route/next-route) n) "control values untouched")))))

(deftest wrap-logging-logs-status-and-errors
  (async done
    (tu/finish done
      (let [log (atom [])
            ok (mw/wrap-logging (fn [_] {:status 201 :body "x"}) {:log #(swap! log conj %)})
            boom (mw/wrap-logging (fn [_] (throw (core/http-error 404 "gone"))) {:log #(swap! log conj %)})
            nxt (mw/wrap-logging (fn [_] nil) {:log #(swap! log conj %)})]
        (p/let [r (ok req)
                e (tu/rejection (boom req))
                _ (nxt req)]
          (is (= {:status 201 :body "x"} r))
          (is (= 404 (util/error-status e)) "errors are re-thrown")
          (is (= [[:get "/x" 201] [:get "/x" 404] [:get "/x" :next]]
                 (mapv (juxt :method :path :status) @log)))
          (is (= "HTTP 404 gone" (:error (second @log))))
          (is (every? number? (map :ms @log))))))))

(deftest wrap-errors->http-error-hides-internals
  (async done
    (tu/finish done
      (let [w (fn [f & [o]] (mw/wrap-errors->http-error f (or o {})))]
        (p/let [plain (tu/rejection ((w (fn [_] (throw (ex-info "db password leaked" {})))) req))
                custom (tu/rejection ((w (fn [_] (throw (tu/js-error "x"))) {:status 502 :message "upstream failed"}) req))
                arg (tu/rejection ((w (fn [_] (throw (util/argument-error "bad input")))) req))
                http (tu/rejection ((w (fn [_] (throw (core/http-error 404 "nope")))) req))
                fine ((w (fn [_] {:body 1})) req)]
          (is (= [:express-cljs/http-error 500 "Internal Server Error"]
                 [(util/error-type plain) (util/error-status plain) (:message (ex-data plain))]))
          (is (some? (:cause (ex-data plain))) "original kept as :cause")
          (is (= [502 "upstream failed"] [(util/error-status custom) (:message (ex-data custom))]))
          (is (= [400 "express-cljs: bad input"] [(util/error-status arg) (:message (ex-data arg))]))
          (is (= [404 "nope"] [(util/error-status http) (:message (ex-data http))]) "http-errors untouched")
          (is (= {:body 1} fine)))))))

(deftest wrap-require-header
  (async done
    (tu/finish done
      (let [h (mw/wrap-require-header (fn [_] {:body "ok"}) :x-api-key {:pred #(= "k" %)})]
        (p/let [missing (tu/rejection (h req))
                wrong (tu/rejection (h (assoc req :headers {:x-api-key "z"})))
                ok (h (assoc req :headers {:x-api-key "k"}))
                custom (tu/rejection ((mw/wrap-require-header (fn [_] nil) :x-a {:status 428 :message "need x-a"}) req))]
          (is (= [400 :missing-header :x-api-key] ((juxt util/error-status (comp :reason ex-data) (comp :header ex-data)) missing)))
          (is (str/includes? (:message (ex-data missing)) "x-api-key"))
          (is (= 400 (util/error-status wrong)))
          (is (= {:body "ok"} ok))
          (is (= [428 "need x-a"] [(util/error-status custom) (:message (ex-data custom))])))))))

(deftest wrap-bearer-auth
  (async done
    (tu/finish done
      (let [h (mw/wrap-bearer-auth (fn [req] {:body (:auth req)})
                                   {:verify (fn [t] (when (= "good" t) (p/resolved {:user "ann"}))) :realm "r"})]
        (p/let [none (tu/rejection (h req))
                bad (tu/rejection (h (assoc req :headers {:authorization "Bearer nope"})))
                basic (tu/rejection (h (assoc req :headers {:authorization "Basic abc"})))
                ok (h (assoc req :headers {:authorization "bearer good"}))
                keyed ((mw/wrap-bearer-auth (fn [req] {:body (:who req)}) {:verify (fn [_] {:u 1}) :key :who})
                       (assoc req :headers {:authorization "Bearer t"}))]
          (is (= [401 "Bearer realm=\"r\""] [(util/error-status none) (get-in (ex-data none) [:headers :www-authenticate])]))
          (is (= [401 "invalid token"] [(util/error-status bad) (:message (ex-data bad))]))
          (is (= 401 (util/error-status basic)))
          (is (= {:body {:user "ann"}} ok) "ctx merged as :auth; scheme is case-insensitive")
          (is (= {:body {:u 1}} keyed))
          (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(mw/wrap-bearer-auth identity {}))))))))))

(deftest wrap-cors-headers-and-preflight
  (async done
    (tu/finish done
      (let [h (mw/wrap-cors (fn [_] {:body "x"}))
            strict (mw/wrap-cors (fn [_] {:body "x"}) {:origins ["https://a.example"] :credentials? true :max-age 600 :methods [:get] :headers ["x-h"]})]
        (p/let [plain (h req)
                any (h (assoc req :headers {:origin "https://z.example"}))
                pre (h (assoc req :method :options :headers {:origin "https://z.example" :access-control-request-method "POST"}))
                allowed (strict (assoc req :headers {:origin "https://a.example"}))
                denied (strict (assoc req :headers {:origin "https://b.example"}))
                spre (strict (assoc req :method :options :headers {:origin "https://a.example" :access-control-request-method "GET"}))]
          (is (= {:body "x"} plain) "no Origin header: nothing added")
          (is (= {:access-control-allow-origin "*" :vary "Origin"} (:headers any)))
          (is (= 204 (:status pre)))
          (is (= "GET, POST, PUT, PATCH, DELETE, OPTIONS" (get-in pre [:headers :access-control-allow-methods])))
          (is (= "content-type, authorization" (get-in pre [:headers :access-control-allow-headers])))
          (is (= {:access-control-allow-origin "https://a.example" :vary "Origin" :access-control-allow-credentials "true"}
                 (:headers allowed)))
          (is (= {:body "x"} denied) "unlisted origin gets no CORS headers")
          (is (= ["GET" "x-h" "600"] [(get-in spre [:headers :access-control-allow-methods])
                                      (get-in spre [:headers :access-control-allow-headers])
                                      (get-in spre [:headers :access-control-max-age])])))))))

(deftest compose-and-wrap-when
  (async done
    (tu/finish done
      (let [order (atom [])
            tag (fn [k] (fn [h] (fn [r] (swap! order conj k) (h r))))
            h ((mw/compose (tag :outer) (tag :inner)) (fn [_] {:body 1}))
            cond-h (mw/wrap-when (fn [_] {:body "base"}) #(= :post (:method %)) mw/wrap-json)]
        (p/let [r (h req)
                g (cond-h req)
                pst (cond-h (assoc req :method :post))]
          (is (= {:body 1} r))
          (is (= [:outer :inner] @order) "compose runs the first wrapper outermost")
          (is (= {:body "base"} g))
          (is (= {:body "base" :json true} pst)))))))

(deftest wrappers-through-a-real-server
  (async done
    (tu/finish done
      (let [log (atom [])
            handler (-> (fn [req] {:body {:id (:request-id req) :user (:auth req)}})
                        (mw/wrap-bearer-auth {:verify (fn [t] (when (= "tok" t) {:name "bob"}))})
                        (mw/wrap-cors {:origins ["*"]})
                        (mw/wrap-timing)
                        (mw/wrap-logging {:log #(swap! log conj %)}))]
        (tu/with-server
          (core/app {:env :test} {:use [(mw/request-id)] :get {"/me" handler}})
          (fn [_ base]
            (p/let [ok (tu/fetch! (str base "/me") {:headers {:authorization "Bearer tok" :origin "https://x" :x-request-id "rid-1"}})
                    denied (tu/fetch! (str base "/me"))]
              (is (= {:id "rid-1" :user {:name "bob"}} (:json ok)))
              (is (= "rid-1" (get-in ok [:headers :x-request-id])) "request-id echoed via continue headers")
              (is (= "*" (get-in ok [:headers :access-control-allow-origin])))
              (is (re-matches #"\d+(\.\d)?ms" (get-in ok [:headers :x-response-time])))
              (is (= 401 (:status denied)))
              (is (str/starts-with? (get-in denied [:headers :www-authenticate]) "Bearer") "http-error headers reach the wire")
              (is (= [200 401] (mapv :status @log))))))))))
