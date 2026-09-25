(ns express-cljs.route-test
  "Pure tests of the routing-as-data layer: no server, no interop."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [express-cljs.middleware :as mw]
            [express-cljs.route :as route]
            [express-cljs.util :as util]
            [express-cljs.test-util :as tu]))

(defn- h [_] {:body "x"})

(deftest check-path-accepts-express-5-syntax
  (doseq [p ["/" "/users/:id" "/files/*splat" "/opt{/:id}" "/ext/:file{.:ext}" "/quoted/:\"my-name\"" "/files{/*splat}"]]
    (is (= p (route/check-path p)) (str "accepts " p)))
  (is (= ["/a" "/b"] (route/check-path ["/a" "/b"])))
  (is (regexp? (route/check-path #"^/re/(\d+)$"))))

(deftest check-path-rejects-express-4-spellings
  (doseq [[p needle] [["/bad/:id?" "{/:id}"]
                      ["/bad(x)" "RegExp"]
                      ["/bad/*" "*splat"]
                      ["/bad+" "reserved"]
                      ["nope" "start with /"]
                      ["" "blank"]
                      [42 "string, a RegExp"]]]
    (let [e (tu/thrown #(route/check-path p))]
      (is (= :express-cljs/argument-error (util/error-type e)) (str "rejects " (pr-str p)))
      (is (str/includes? (ex-message e) needle) (str "message for " (pr-str p) " mentions " needle ": " (ex-message e))))))

(deftest constructors-produce-ops
  (is (= {:op :route :method :get :path "/x" :handlers [h]} (route/get "/x" h)))
  (is (= [h h] (:handlers (route/post "/x" [h] h))) "middleware vector flattened before the handler")
  (is (= :all (:method (route/all "/x" h))))
  (is (= :query (:method (route/query "/x" h))))
  (is (= {:op :use :path "/" :handlers [h]} (route/use h)))
  (is (= {:op :use :path "/api" :handlers [h h]} (route/use "/api" h h)))
  (is (= {:op :param :name "id" :handler h} (route/param "id" h)))
  (let [r (route/router "/admin" {:merge-params? true} (route/get "/" h))]
    (is (= :router (:op r)))
    (is (= {:merge-params true} (:opts r)))
    (is (= [{:op :route :method :get :path "/" :handlers [h]}] (:plan r))))
  (is (= {} (:opts (route/router "/admin" (route/get "/" h)))) "options map optional")
  (let [r (route/resource "/things" {:get h :post [h h]})]
    (is (= :resource (:op r)))
    (is (= {:get [h] :post [h h]} (:methods r))))
  (let [s (route/static "/static" "/srv/pub" {:dotfiles :allow})]
    (is (= :use (:op s)))
    (is (= {:express-cljs/builtin :static :root "/srv/pub" :opts {:dotfiles :allow}} (first (:handlers s)))))
  (let [e (route/error "/api" h)]
    (is (= :use (:op e)))
    (is (= {:express-cljs/error-handler h} (first (:handlers e)))))
  (is (= {:express-cljs/upstream h} (route/upstream h))))

(deftest constructors-validate
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/route :bogus "/x" h)))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/get "/x")))) "handler required")
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/get "/x" "not-a-handler")))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/param "" h)))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/router "/a" {:bogus 1} (route/get "/" h))))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/resource "/a" {})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/resource "/a" {:bogus h})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/error-handler "f")))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/static "/s" ""))))))

(deftest control-values
  (is (= {:express-cljs/next :route} (route/next-route)))
  (is (= {:express-cljs/next :router} (route/next-router)))
  (is (= {:express-cljs/next :next} (route/continue)))
  (is (= {:express-cljs/next :next :assoc {:user 1} :headers {:x-id "1"}} (route/continue {:user 1} {:x-id "1"})))
  (is (route/control? (route/continue {:a 1})))
  (is (not (route/control? {:body "x"})))
  (is (not (route/control? nil))))

(defn- before? [a b] (neg? (route/specificity-compare (route/specificity a) (route/specificity b))))

(deftest specificity-orders-literal-before-param-before-wildcard
  (is (before? "/users/me" "/users/:id"))
  (is (before? "/files/a/b" "/files/*splat") "a longer literal path beats a shorter wildcard one")
  (is (before? "/opt/x" "/opt{/:id}"))
  (is (before? "/a/b/:y" "/a/:x/c") "first differing segment decides")
  (is (before? "/users" "/users/me") "a strict prefix sorts first")
  (is (before? "/z" #"^/re$") "RegExps last")
  (is (before? "/users/:id" "/files/*splat") "parameter before wildcard")
  (is (= (route/specificity "/b") (route/specificity ["/b" "/:x"])) "vector uses its most specific member"))

(deftest table-form-compiles-in-kind-and-specificity-order
  (let [p1 (fn [_ _] nil)
        eh (fn [_ _] nil)
        plan (route/compile {:get {"/users/:id" h "/users/me" h "/files/*splat" h "/files/a/b" h}
                             :post {"/users" h}
                             :all {"/users/me" h}
                             :use [(mw/json) ["/api" h]]
                             :param {"id" p1}
                             :routers [{:mount "/admin" :opts {:merge-params? true} :routes {:get {"/" h}}}]
                             :resource {"/things" {:get h}}
                             :error [eh]})]
    (is (= [[:param "id"]
            [:use "/"]
            [:use "/api"]
            [:get "/admin/"]
            [:post "/users"]
            [:get "/users/me"]
            [:all "/users/me"]
            [:get "/files/a/b"]
            [:get "/users/:id"]
            [:get "/files/*splat"]
            [:get "/things"]
            [:use "/"]]
           (route/plan-paths plan)))
    (is (= {:express-cljs/builtin :json :opts {}} (get-in plan [1 :handlers 0])) "descriptor kept as data")
    (is (= {:express-cljs/error-handler eh} (get-in (peek plan) [:handlers 0])) "error entries become error handlers")))

(deftest table-form-use-spellings
  (is (= [[:use "/api"] [:use "/api"]]
         (route/plan-paths (route/compile {:use ["/api" h h]})))
      "leading path string applies to every following middleware")
  (is (= [[:use "/"] [:use "/x"] [:get "/m/"]]
         (route/plan-paths (route/compile {:use [h ["/x" h] {:mount "/m" :routes {:get {"/" h}}}]})))
      "vector entries carry their own path; router maps nest"))

(deftest table-form-rejects-unknown-keys
  (let [e (tu/thrown #(route/compile {:gett {"/x" h}}))]
    (is (= :express-cljs/argument-error (util/error-type e)))
    (is (= #{:gett} (:unknown (ex-data e))))))

(deftest ordered-form-keeps-source-order
  (let [plan (route/compile (route/routes (route/get "/users/:id" h)
                                          (route/get "/users/me" h))
                            [[:post "/p" h] [:use "/u" h]]
                            (route/get "/last" h))]
    (is (= [[:get "/users/:id"] [:get "/users/me"] [:post "/p"] [:use "/u"] [:get "/last"]]
           (route/plan-paths plan))
        "no specificity sort in the ordered form; tuples and vectors accepted")))

(deftest tuple-shorthand
  (is (= [[:get "/a"] [:use "/b"] [:param "id"] [:get "/r/x"] [:post "/res"] [:use "/e"] [:use "/s"]]
         (route/plan-paths (route/compile [[:get "/a" h]
                                           [:use "/b" h]
                                           [:param "id" h]
                                           [:router "/r" {:strict? true} {:get {"/x" h}}]
                                           [:resource "/res" {:post h}]
                                           [:error "/e" h]
                                           [:static "/s" "/srv"]]))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/compile [[:nope "/a" h]]))))))

(deftest compile-is-total
  (is (= [] (route/compile)))
  (is (= [] (route/compile nil [])))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/compile "string")))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(route/compile {:get {"/bad/:id?" h}}))))
      "path lint runs at compile time"))

(deftest plan-paths-flattens-nested-routers
  (is (= [[:get "/a/b/c"] [:use "/a/b/"]]
         (route/plan-paths (route/compile (route/router "/a" (route/router "/b" (route/get "/c" h) (route/use h))))))))

(deftest methods-set-matches-inventory
  (is (= 35 (count route/methods)))
  (is (contains? route/methods :query))
  (is (contains? route/methods :m-search)))
