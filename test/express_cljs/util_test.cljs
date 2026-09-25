(ns express-cljs.util-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [express-cljs.util :as util]
            [express-cljs.test-util :as tu]))

(deftest kebab-camel-roundtrip
  (is (= "maxAge" (util/kebab->camel :max-age)))
  (is (= "perMessageDeflate" (util/kebab->camel :per-message-deflate)))
  (is (= "limit" (util/kebab->camel :limit)) "single word unchanged")
  (is (= "alreadyCamel" (util/kebab->camel "alreadyCamel")) "strings pass through")
  (is (= "maxAge" (util/kebab->camel :express-cljs/max-age)) "namespace dropped")
  (is (= :max-age (util/camel->kebab "maxAge")))
  (is (= :per-message-deflate (util/camel->kebab :perMessageDeflate)))
  (is (= :last-modified (util/camel->kebab "lastModified")))
  (is (= :limit (util/camel->kebab "limit")))
  (is (= :express-cljs/x (util/camel->kebab :express-cljs/x)) "namespaced keywords untouched")
  (doseq [k [:max-age :per-message-deflate :max-payload :client-tracking :dotfiles
             :fallthrough :last-modified :set-headers :same-site :http-only :inflate
             :parameter-limit :extended :case-sensitive :strict :merge-params]]
    (is (= k (util/camel->kebab (util/kebab->camel k))) (str "roundtrip " k))))

(deftest transform-keys-honours-passthrough
  (is (= {"aB" {"cD" 1} "body" {:x-y 2}}
         (util/transform-keys util/kebab->camel #{:body} {:a-b {:c-d 1} :body {:x-y 2}})))
  (is (= [{:a-b 1}] (util/transform-keys util/camel->kebab [{:aB 1}]))))

(deftest ->camel-keys-drops-nil-and-keeps-user-data
  (let [o (util/->camel-keys {:max-age 1000 :http-only true :same-site :lax :nil-key nil
                              :headers {:X-Custom 1} :body {:user-key 1}
                              :nested {:per-message-deflate false}})]
    (is (= {"maxAge" 1000 "httpOnly" true "sameSite" :lax
            "headers" {:X-Custom 1} "body" {:user-key 1}
            "nested" {"perMessageDeflate" false}}
           o)
        "keys camelised, nil dropped, values untouched, passthrough subtrees kept")
    (is (nil? (util/->camel-keys nil)))))

(deftest ->kebab-keys-translates-upstream-maps
  (is (= {:max-payload 1 :per-message-deflate {:zlib-deflate-options {:level 3}}}
         (util/->kebab-keys {"maxPayload" 1 "perMessageDeflate" {"zlibDeflateOptions" {"level" 3}}}))))

(deftest enum-values
  (is (= "lax" (util/enum-value :lax)))
  (is (= "sameOrigin" (util/enum-value :same-origin)))
  (is (= 5 (util/enum-value 5)))
  (is (= "x" (util/enum-value "x"))))

(deftest app-settings-are-space-separated
  (is (= "trust proxy" (util/kebab->setting :trust-proxy)))
  (is (= "case sensitive routing" (util/kebab->setting :case-sensitive-routing)))
  (is (= "json spaces" (util/kebab->setting :json-spaces)))
  (is (= "x-powered-by" (util/kebab->setting :x-powered-by)) "the one dashed setting keeps its dash")
  (is (= "etag" (util/kebab->setting :etag)))
  (is (= "env" (util/kebab->setting "env")) "strings pass through")
  (is (= :trust-proxy (util/setting->kebab "trust proxy")))
  (is (= :x-powered-by (util/setting->kebab "x-powered-by")))
  (is (= :view-engine (util/setting->kebab "View  Engine")) "case and runs of spaces normalised"))

(deftest header-names-are-lower-case-kebab-keywords
  (is (= "content-type" (util/header-name :Content-Type)))
  (is (= "x-request-id" (util/header-name "X-Request-Id")))
  (is (= :content-type (util/header-keyword "Content-Type")))
  (is (= :content-type (util/header-keyword :Content-Type)))
  (is (= {:content-type "text/plain" :set-cookie ["a=1" "b=2"]}
         (util/->header-map {"Content-Type" "text/plain" "set-cookie" ["a=1" "b=2"]}))
      "values untouched")
  (is (= {} (util/->header-map nil)))
  (is (= {"content-type" "application/json" "transfer-encoding" "chunked" "x-count" "3" "vary" ["accept" "origin"]}
         (util/->wire-headers {:Content-Type "application/json" :Transfer-Encoding :chunked :x-count 3
                               :vary [:accept "origin"] :x-nil nil}))
      "string keys lower-cased, keyword values rendered, numbers stringified, nil dropped")
  (is (= {} (util/->wire-headers nil))))

(deftest reason-phrases-and-status-validation
  (is (= "Not Found" (util/reason-phrase 404)))
  (is (= "Payload Too Large" (util/reason-phrase 413)))
  (is (= "599" (util/reason-phrase 599)) "unknown codes print as the number")
  (is (util/valid-status? 200))
  (is (util/valid-status? 599))
  (is (not (util/valid-status? 99)))
  (is (not (util/valid-status? 600)))
  (is (not (util/valid-status? "404")))
  (is (not (util/valid-status? 404.5))))

(deftest normalize-opts-and-check-opts
  (is (= {:case-sensitive true :strict false} (util/normalize-opts {:case-sensitive? true :strict false :x nil})))
  (is (= {} (util/normalize-opts nil)))
  (is (= :express-cljs/a (util/strip-question-mark :express-cljs/a?)))
  (is (= "s" (util/strip-question-mark "s")))
  (is (= {:limit "1mb"} (util/check-opts! :json #{:limit :strict} {:limit "1mb"})))
  (let [e (tu/thrown #(util/check-opts! :json #{:limit :strict} {:limit "1mb" :bogus 1}))]
    (is (= :express-cljs/argument-error (util/error-type e)))
    (is (= #{:bogus} (:unknown (ex-data e))))
    (is (str/includes? (ex-message e) "json: unknown option(s) [:bogus]")))
  (let [e (tu/thrown #(util/check-opts! :json #{} "nope"))]
    (is (= :express-cljs/argument-error (util/error-type e)))))

(deftest http-error-constructor
  (let [e (util/http-error 404)
        e2 (util/http-error 503 "down" {:headers {:retry-after "5"}})
        e3 (util/http-error 422 "bad body" {:expose? false :body {:errors [1]}})]
    (is (= :express-cljs/http-error (util/error-type e)))
    (is (= 404 (:status (util/error-data e))))
    (is (= "Not Found" (:message (util/error-data e))) "message defaults to the reason phrase")
    (is (= "HTTP 404 Not Found" (ex-message e)))
    (is (true? (:expose? (ex-data e))) "4xx exposed by default")
    (is (false? (:expose? (ex-data e2))) "5xx hidden by default")
    (is (= {:retry-after "5"} (:headers (ex-data e2))))
    (is (= "HTTP 503 down" (ex-message e2)))
    (is (false? (:expose? (ex-data e3))) "explicit :expose? wins")
    (is (= {:errors [1]} (:body (ex-data e3))))
    (is (= 404 (util/error-status e)))
    (doseq [bad [200 399 600 "404" nil]]
      (let [err (tu/thrown #(util/http-error bad))]
        (is (= :express-cljs/argument-error (util/error-type err)) (str "rejects " (pr-str bad)))))))

(deftest server-and-ws-errors
  (let [s (util/server-error "EADDRINUSE" "address in use" {:op :listen :port 3000})
        s2 (util/server-error nil "closed")
        w (util/ws-error "socket closed" {:code 1006 :reason ""})]
    (is (= :express-cljs/server-error (util/error-type s)))
    (is (= :eaddrinuse (:code (util/error-data s))))
    (is (= :listen (:op (ex-data s))))
    (is (= "express-cljs eaddrinuse: address in use" (ex-message s)))
    (is (nil? (:code (ex-data s2))))
    (is (= "express-cljs: closed" (ex-message s2)))
    (is (= :express-cljs/ws-error (util/error-type w)))
    (is (= 1006 (:code (ex-data w))))
    (is (str/starts-with? (ex-message w) "express-cljs ws:"))
    (is (= 500 (util/error-status s)) "non-http errors default to 500")
    (is (= 500 (util/error-status w)))))

(deftest rpc-errors-map-to-codes-and-statuses
  (let [e (util/rpc-error :method-not-found "no such method" {:id 7 :data {:method "x"}})]
    (is (= :express-cljs/rpc-error (util/error-type e)))
    (is (= :method-not-found (util/rpc-error-code e)))
    (is (= 7 (:id (ex-data e))))
    (is (= 404 (util/error-status e)))
    (is (= -32601 (get-in util/rpc-error-codes [:method-not-found :code])))
    (is (= "express-cljs rpc method-not-found: no such method" (ex-message e))))
  (is (= 400 (util/error-status (util/rpc-error :invalid-params "x"))))
  (is (= 500 (util/error-status (util/rpc-error :internal-error "x"))))
  (is (= 401 (util/error-status (util/rpc-error :unauthorized "x"))))
  (is (nil? (util/rpc-error-code (util/http-error 404))))
  (let [err (tu/thrown #(util/rpc-error :nope "x"))]
    (is (= :express-cljs/argument-error (util/error-type err)))))

(deftest error-status-honours-foreign-status-fields
  (is (= 400 (util/error-status (util/argument-error "bad"))))
  (is (= 418 (util/error-status (ex-info "teapot" {:status 418}))))
  (is (= 409 (util/error-status (ex-info "conflict" {:status-code 409}))))
  (is (= 500 (util/error-status (ex-info "ok-status-ignored" {:status 200}))))
  (is (= 500 (util/error-status (ex-info "plain" {}))))
  (is (= 500 (util/error-status (js/Error. "plain")))))

(deftest error-detection
  (is (nil? (util/error-data (ex-info "other" {:type :something/else}))))
  (is (nil? (util/error-data (js/Error. "plain"))))
  (is (nil? (util/error-type nil)))
  (is (= 5 (count util/error-types))))
