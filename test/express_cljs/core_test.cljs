(ns express-cljs.core-test
  "The binding: pure translation tests plus real loopback round-trips (an
   ephemeral 127.0.0.1 port per test, closed by the fixture). Interop-free:
   every JS touch is in test-util or core."
  (:require [cljs.core.async :as async]
            [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [promesa.core :as p]
            [express-cljs.core :as core]
            [express-cljs.middleware :as mw]
            [express-cljs.route :as route]
            [express-cljs.util :as util]
            [express-cljs.test-util :as tu]))

(defn- app* [& sources] (apply core/app {:env :test} sources))

(defn- error-of
  "The :error map of a wrapper error body."
  [resp]
  (get-in resp [:json :error]))

;; ---------------------------------------------------------------------------
;; Pure translation
;; ---------------------------------------------------------------------------

(deftest trust-proxy-spellings
  (is (false? (util/trust-proxy-setting nil)))
  (is (true? (util/trust-proxy-setting true)))
  (is (= 2 (util/trust-proxy-setting 2)))
  (is (= "loopback" (util/trust-proxy-setting :loopback)))
  (is (= "10.0.0.0/8" (util/trust-proxy-setting "10.0.0.0/8")))
  (is (= "loopback, 10.0.0.0/8" (util/trust-proxy-setting [:loopback "10.0.0.0/8"])))
  (let [f (fn [_ip _i] true)] (is (identical? f (util/trust-proxy-setting f))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(util/trust-proxy-setting {:a 1}))))))

(deftest app-options-become-settings
  (is (= {"x-powered-by" false} (util/app-settings {})) "only the wrapper default when nothing is given")
  (is (= {"x-powered-by" true "env" "production" "trust proxy" "loopback" "case sensitive routing" true
          "strict routing" false "query parser" "extended" "etag" "strong" "json spaces" 2 "subdomain offset" 3
          "json escape" true}
         (util/app-settings {:x-powered-by? true :env :production :trust-proxy :loopback :case-sensitive-routing? true
                             :strict-routing? false :query-parser :extended :etag :strong :json-spaces 2
                             :subdomain-offset 3 :json-escape? true})))
  (is (= false (get (util/app-settings {:etag false}) "etag")))
  (let [a (core/app {:env :test :trust-proxy [:loopback "10.0.0.0/8"] :query-parser :extended :etag false})]
    (is (= "test" (core/setting a :env)))
    (is (= "loopback, 10.0.0.0/8" (core/setting a :trust-proxy)))
    (is (= "extended" (core/setting a :query-parser)))
    (is (false? (core/setting a :etag)))
    (is (false? (core/setting a :x-powered-by)) "X-Powered-By off by default (pitfall 9.14)")
    (core/set-setting! a :json-spaces 4)
    (is (= 4 (core/setting a :json-spaces))))
  (let [e (tu/thrown #(core/app {:bogus 1}))]
    (is (= :express-cljs/argument-error (util/error-type e)))
    (is (= #{:bogus} (:unknown (ex-data e))))))

(deftest cookie-header-parsing
  (is (= {} (core/parse-cookies nil)))
  (is (= {:a "1" :b "x y" :c "q\"uoted"}
         (core/parse-cookies "a=1; b=x%20y; c=\"q%22uoted\"; a=dup; novalue"))
      "first occurrence wins, quotes stripped, URI-decoded, pairs without = skipped")
  (is (= {:raw "%E0%A4%A"} (core/parse-cookies "raw=%E0%A4%A")) "malformed escape keeps the raw value"))

(deftest cookie-signing-is-byte-compatible-with-cookie-signature
  (let [secret "s3cret"]
    (doseq [v ["hello" "with.dots" "j:{\"a\":1}" ""]]
      (is (= (tu/cookie-signature-sign v secret) (core/cookie-sign v secret)) (str "sign " (pr-str v)))
      (is (= v (core/cookie-unsign (tu/cookie-signature-sign v secret) secret)) (str "unsign theirs " (pr-str v)))
      (is (= v (tu/cookie-signature-unsign (core/cookie-sign v secret) secret)) (str "they unsign ours " (pr-str v))))
    (is (false? (core/cookie-unsign (str "tampered" (subs (core/cookie-sign "hello" secret) 5)) secret)))
    (is (false? (core/cookie-unsign "nodot" secret)))
    (is (false? (core/cookie-unsign (core/cookie-sign "hello" secret) "other-secret")))
    (is (= [{:plain "1" :obj {:k [1 2]}} {:sid "abc" :bad false}]
           (core/cookies-and-signed {:plain "1"
                                     :obj "j:{\"k\":[1,2]}"
                                     :sid (str "s:" (core/cookie-sign "abc" secret))
                                     :bad "s:abc.nope"}
                                    secret))
        "signed values move to :signed-cookies, tampered -> false, j: decoded")
    (is (= [{:sid "s:abc.x"} {}] (core/cookies-and-signed {:sid "s:abc.x"} nil)) "no secret: nothing moves")))

(deftest response-map-validation
  (is (= {:status 200 :body "x"} (util/check-response! {:status 200 :body "x"})))
  (doseq [bad [{:bogus 1} {:body "a" :redirect "/b"} {:status 99} {:status "200"} {:cookies [{:value 1}]} "nope"]]
    (let [e (tu/thrown #(util/check-response! bad))]
      (is (= :express-cljs/server-error (util/error-type e)) (str "rejects " (pr-str bad)))
      (is (= :invalid-response (:code (ex-data e)))))))

(deftest error-rendering-is-pure
  (let [json-req {:accepts ["*/*"] :method :get}
        html-req {:accepts ["text/html" "*/*"]}]
    (is (util/accepts-json? json-req))
    (is (not (util/accepts-json? html-req)))
    (is (util/accepts-json? {:accepts ["text/html"] :content-type "application/json"}) "a JSON request gets JSON errors")
    (is (util/accepts-json? {:accepts []}) "no Accept header at all -> JSON")
    (let [r (util/error-response (util/http-error 422 "bad body" {:reason :validation :headers {:x-a "1"}}) json-req {:env :test})]
      (is (= 422 (:status r)))
      (is (= {:error {:status 422 :message "bad body" :reason :validation}} (:body r)))
      (is (true? (:json r)))
      (is (= "1" (get-in r [:headers :x-a])))
      (is (= "nosniff" (get-in r [:headers :x-content-type-options]))))
    (is (= "422 bad body" (:body (util/error-response (util/http-error 422 "bad body") html-req {:env :test}))) "text for HTML clients")
    (is (= {:errors [1]} (:body (util/error-response (util/http-error 422 "x" {:body {:errors [1]}}) json-req {}))) "custom :body wins")
    (is (= "Service Unavailable" (get-in (util/error-response (util/http-error 503 "db down") json-req {}) [:body :error :message]))
        "5xx http-error messages hidden unless :expose? true")
    (is (= "db down" (get-in (util/error-response (util/http-error 503 "db down" {:expose? true}) json-req {}) [:body :error :message])))
    (is (= "boom" (get-in (util/error-response (ex-info "boom" {}) json-req {:env :development}) [:body :error :message]))
        "foreign errors show their message outside production")
    (is (= "Internal Server Error" (get-in (util/error-response (ex-info "boom" {}) json-req {:env :production}) [:body :error :message])))
    (is (= "boom" (get-in (util/error-response (ex-info "boom" {}) json-req {:env :production :expose-errors? true}) [:body :error :message])))
    (is (= 418 (:status (util/error-response (ex-info "tea" {:status 418}) json-req {}))))
    (is (= 400 (:status (util/error-response (util/argument-error "bad arg") json-req {}))))))

(deftest fresh-and-stale
  (let [req {:headers {:if-none-match "\"abc\""}}]
    (is (core/fresh? req {:etag "\"abc\""}))
    (is (core/fresh? req {:etag "W/\"abc\""}) "weak comparison")
    (is (core/stale? req {:etag "\"other\""}))
    (is (core/fresh? {:headers {:if-none-match "*"}} {}))
    (is (core/stale? {:headers {:if-none-match "\"abc\"" :cache-control "no-cache"}} {:etag "\"abc\""}))
    (is (core/stale? {:headers {}} {:etag "\"abc\""}) "unconditional request")
    (is (core/fresh? {:headers {:if-modified-since "Wed, 21 Oct 2015 07:28:00 GMT"}} {:last-modified "Tue, 20 Oct 2015 07:28:00 GMT"}))
    (is (core/stale? {:headers {:if-modified-since "Mon, 19 Oct 2015 07:28:00 GMT"}} {:last-modified "Tue, 20 Oct 2015 07:28:00 GMT"}))
    (is (core/fresh? (:headers req) {:etag "\"abc\""}) "a bare header map works too")))

(deftest methods-come-from-node
  (is (contains? core/methods :get))
  (is (contains? core/methods :query) "Node 22 exposes QUERY (inventory 3.8)")
  (is (= route/methods core/methods) "the static set in route matches this runtime"))

(deftest error-normalisation
  (is (= :express-cljs/http-error (util/error-type (core/->error (util/http-error 404)))))
  (let [e (core/->error (ex-info "plain" {:status 418}))]
    (is (= 418 (util/error-status e)) "foreign ex-info passes through"))
  (let [e (core/->error "just a string")]
    (is (= :express-cljs/server-error (util/error-type e)))
    (is (= "express-cljs: just a string" (ex-message e))))
  (let [e (core/->error nil)]
    (is (= :rejected-falsy (:code (ex-data e)))))
  (let [e (core/->error false)]
    (is (= :rejected-falsy (:code (ex-data e))))))

;; ---------------------------------------------------------------------------
;; Real requests
;; ---------------------------------------------------------------------------

(deftest request-map-and-header-casing-both-ways
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/get "/echo/:id" (fn [req]
                                       {:status 201
                                        :headers {:X-Resp-Header "yes" :x-multi ["1" "2"]}
                                        :body (-> req
                                                  (select-keys [:method :url :original-url :base-url :path :params :query
                                                                :headers :host :hostname :protocol :secure? :ip :ips :xhr?
                                                                :http-version :remote-address :accepts :content-length
                                                                :route :app-env :transport :body :cookies :signed-cookies])
                                                  (update :headers select-keys [:x-custom-header :accept :cookie]))})))
        (fn [_ base]
          (p/let [r (tu/fetch! (str base "/echo/42?a=1&a=2&b[c]=3")
                               {:headers {:X-Custom-Header "V" :accept "application/json" :cookie "k=v"
                                          :x-requested-with "XMLHttpRequest"}})
                  j (:json r)]
            (is (= 201 (:status r)))
            (is (= "yes" (get-in r [:headers :x-resp-header])) "response header lower-cased on the wire and read back as kebab")
            (is (= "1, 2" (get-in r [:headers :x-multi])) "vector value sent as repeated header (folded by the client)")
            (is (= "application/json; charset=utf-8" (get-in r [:headers :content-type])))
            (is (nil? (get-in r [:headers :x-powered-by])))
            (is (= "get" (:method j)))
            (is (= "/echo/42?a=1&a=2&b[c]=3" (:url j)))
            (is (= (:url j) (:original-url j)))
            (is (= "" (:base-url j)))
            (is (= "/echo/42" (:path j)))
            (is (= {:id "42"} (:params j)))
            (is (= {:a ["1" "2"] (keyword "b[c]") "3"} (:query j)) "simple parser: repeated keys -> vector, brackets literal")
            (is (= {:x-custom-header "V" :accept "application/json" :cookie "k=v"} (:headers j)) "request header names lower-case kebab")
            (is (= "127.0.0.1" (:hostname j)))
            (is (str/starts-with? (:host j) "127.0.0.1:") "Express 5 req.host keeps the port")
            (is (= "http" (:protocol j)))
            (is (false? (:secure? j)))
            (is (= "127.0.0.1" (:ip j)))
            (is (= [] (:ips j)))
            (is (true? (:xhr? j)))
            (is (= "1.1" (:http-version j)))
            (is (= "127.0.0.1" (:remote-address j)))
            (is (= ["application/json"] (:accepts j)))
            (is (nil? (:content-length j)))
            (is (= {:path "/echo/:id"} (:route j)))
            (is (= "test" (:app-env j)))
            (is (= "http" (:transport j)))
            (is (nil? (:body j)) "no parser ran -> :body nil")
            (is (= {:k "v"} (:cookies j)))
            (is (= {} (:signed-cookies j)))))))))

(deftest route-matching-params-wildcards-and-404s
  (async done
    (tu/finish done
      (tu/with-server
        (app* {:get {"/users/:id" (fn [{:keys [params]}] {:body {:route "param" :params params}})
                     "/users/me" (fn [_] {:body {:route "me"}})
                     "/files/*splat" (fn [{:keys [params]}] {:body {:params params}})
                     "/opt{/:id}" (fn [{:keys [params]}] {:body {:params params}})
                     "/ext/:file{.:ext}" (fn [{:keys [params]}] {:body {:params params}})
                     "/text" (fn [_] {:type :text :body "plain"})}
               :post {"/only-post" (fn [_] {:body {:ok true}})}
               :resource {"/things" {:get (fn [_] {:body {:things []}})
                                     :post (fn [_] {:status 201 :body {:created true}})}}})
        (fn [_ base]
          (p/let [me (tu/fetch! (str base "/users/me"))
                  by-id (tu/fetch! (str base "/users/7/"))
                  deep (tu/fetch! (str base "/users/7/extra"))
                  upper (tu/fetch! (str base "/USERS/me"))
                  splat (tu/fetch! (str base "/files/a/b/c"))
                  bare (tu/fetch! (str base "/files"))
                  opt0 (tu/fetch! (str base "/opt"))
                  opt1 (tu/fetch! (str base "/opt/9"))
                  ext (tu/fetch! (str base "/ext/report.pdf"))
                  noext (tu/fetch! (str base "/ext/report"))
                  nope (tu/fetch! (str base "/nope"))
                  wrong (tu/fetch! (str base "/only-post"))
                  head (tu/fetch! (str base "/text") {:method :head})
                  options (tu/fetch! (str base "/text") {:method :options})
                  things (tu/fetch! (str base "/things"))
                  del (tu/fetch! (str base "/things") {:method :delete})
                  html404 (tu/fetch! (str base "/nope") {:headers {:accept "text/html"}})]
            (is (= "me" (get-in me [:json :route])) "table form: /users/me registered before /users/:id")
            (is (= {:route "param" :params {:id "7"}} (:json by-id)) "trailing slash optional")
            (is (= 404 (:status deep)) "params stop at the segment delimiter")
            (is (= "me" (get-in upper [:json :route])) "case-insensitive by default")
            (is (= {:params {:splat ["a" "b" "c"]}} (:json splat)) "wildcard -> vector of segments")
            (is (= 404 (:status bare)) "/files/*splat needs >= 1 segment (pitfall 9.3)")
            (is (= {:params {}} (:json opt0)) "unmatched optional param omitted")
            (is (= {:params {:id "9"}} (:json opt1)))
            (is (= {:params {:file "report" :ext "pdf"}} (:json ext)))
            (is (= {:params {:file "report"}} (:json noext)))
            (is (= 404 (:status nope)))
            (is (= {:status 404 :message "Not Found" :reason "not-found"} (error-of nope)) "wrapper 404 as JSON")
            (is (= 404 (:status wrong)) "wrong method on a known path is a 404 (pitfall 9.4)")
            (is (= 200 (:status head)))
            (is (= "" (:body head)) "HEAD served by the GET route without a body")
            (is (= 200 (:status options)))
            (is (= "GET, HEAD" (get-in options [:headers :allow])) "automatic OPTIONS answer still works")
            (is (= {:things []} (:json things)))
            (is (= 405 (:status del)) "resource answers 405 for other methods")
            (is (= "GET, POST, HEAD" (get-in del [:headers :allow])))
            (is (= 404 (:status html404)))
            (is (str/starts-with? (get-in html404 [:headers :content-type]) "text/plain"))
            (is (= "404 Not Found" (:body html404)) "text rendering for non-JSON clients")))))))

(deftest routing-and-registration-errors-surface-as-argument-errors
  (let [e (tu/thrown #(app* (route/get "/x" (fn [_] nil)) [:get "/bad/:id?" (fn [_] nil)]))]
    (is (= :express-cljs/argument-error (util/error-type e)))
    (is (str/includes? (ex-message e) "{/:id}")))
  (let [e (tu/thrown #(app* {:op :route :method :get :path "/x/:" :handlers [(fn [_] nil)]}))]
    (is (= :express-cljs/argument-error (util/error-type e)) "an upstream path-to-regexp throw is re-thrown as argument-error")
    (is (str/includes? (ex-message e) "route registration failed"))))

(deftest routers-mounting-and-merge-params
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/router "/mount/:mid" {:merge-params? true}
                (route/use (fn [_] (route/continue {:via "router-mw"})))
                (route/get "/deep/:x" (fn [req] {:body (select-keys req [:url :base-url :path :original-url :params :via])})))
              {:routers [{:mount "/plain/:mid" :routes {:get {"/x" (fn [{:keys [params]}] {:body {:params params}})}}}]}
              (route/get "/top" (fn [req] {:body {:base-url (:base-url req)}})))
        (fn [_ base]
          (p/let [r (tu/fetch! (str base "/mount/M1/deep/X?q=1"))
                  plain (tu/fetch! (str base "/plain/M1/x"))
                  top (tu/fetch! (str base "/top"))]
            (is (= {:url "/deep/X?q=1" :base-url "/mount/M1" :path "/deep/X" :original-url "/mount/M1/deep/X?q=1"
                    :params {:mid "M1" :x "X"} :via "router-mw"}
                   (:json r))
                "mount-relative :url / :path, :base-url with params substituted, mergeParams, continue context")
            (is (= {:params {}} (:json plain)) "without :merge-params? the parent param is invisible")
            (is (= {:base-url ""} (:json top)))))))))

(deftest next-route-next-router-and-continue-semantics
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/routes
                (route/get "/chain" (fn [_] (route/continue nil {:x-first "1"}))
                                    (fn [_] (route/next-route))
                                    (fn [_] {:body "second-handler-should-be-skipped"}))
                (route/get "/chain" (fn [_] {:body "second-route"}))
                (route/router "/r" (route/use (fn [_] (route/next-router)))
                                   (route/get "/x" (fn [_] {:body "inner"})))
                (route/get "/r/x" (fn [_] {:body "outer"}))
                (route/use "/ctx" (fn [_] (route/continue {:user {:name "ann"}})))
                (route/get "/ctx" (fn [req] {:body {:user (:user req)}}))
                (route/get "/nil" (fn [_] nil))))
        (fn [_ base]
          (p/let [chain (tu/fetch! (str base "/chain"))
                  rx (tu/fetch! (str base "/r/x"))
                  ctx (tu/fetch! (str base "/ctx"))
                  nil-r (tu/fetch! (str base "/nil"))]
            (is (= "second-route" (:body chain)) "next-route skips the rest of the route")
            (is (= "1" (get-in chain [:headers :x-first])) "headers set by continue survive")
            (is (= "outer" (:body rx)) "next-router leaves the router")
            (is (= {:user {:name "ann"}} (:json ctx)) "continue merges keys into later request maps")
            (is (= 404 (:status nil-r)) "nil from every handler falls through to the wrapper 404")))))))

(deftest param-loaders
  (async done
    (tu/finish done
      (tu/with-server
        (app* {:param {"id" (fn [_ v] (if (= "boom" v)
                                        (throw (core/http-error 400 "bad id"))
                                        (route/continue {:loaded (str "user-" v)})))}
               :get {"/u/:id" (fn [req] {:body {:loaded (:loaded req)}})}})
        (fn [_ base]
          (p/let [ok (tu/fetch! (str base "/u/5"))
                  bad (tu/fetch! (str base "/u/boom"))]
            (is (= {:loaded "user-5"} (:json ok)))
            (is (= 400 (:status bad)))
            (is (= "bad id" (:message (error-of bad))))))))))

(deftest query-parser-modes
  (async done
    (tu/finish done
      (let [h (fn [{:keys [query]}] {:body {:query query}})]
        (p/do
          (tu/with-server (core/app {:env :test :query-parser :extended} (route/get "/q" h))
            (fn [_ base]
              (p/let [r (tu/fetch! (str base "/q?a=1&b[c]=2&arr=1&arr=2"))]
                (is (= {:query {:a "1" :b {:c "2"} :arr ["1" "2"]}} (:json r)) "extended nests brackets"))))
          (tu/with-server (core/app {:env :test :query-parser false} (route/get "/q" h))
            (fn [_ base]
              (p/let [r (tu/fetch! (str base "/q?a=1"))]
                (is (= {:query {}} (:json r)) "disabled parser")))))))))

(deftest body-parsers
  (async done
    (tu/finish done
      (tu/with-server
        (core/app {:env :test :json {:limit "1kb"} :urlencoded true :text true :raw true}
                  (route/post "/echo" (fn [{:keys [body]}]
                                        {:body {:body (cond (nil? body) nil (string? body) body (map? body) body :else (tu/buffer->string body))
                                                :kind (cond (map? body) "map" (string? body) "string" (nil? body) "nil" :else "buffer")}}))
                  (route/post "/strict-off" (mw/json {:strict? false :type "application/vnd.x+json"})
                              (fn [{:keys [body]}] {:body {:body body}})))
        (fn [_ base]
          (p/let [j (tu/fetch! (str base "/echo") {:method :post :body {:camelKey 1 :x_y [1 2]}})
                  bad (tu/fetch! (str base "/echo") {:method :post :body "{not json" :headers {:content-type "application/json"}})
                  big (tu/fetch! (str base "/echo") {:method :post :headers {:content-type "application/json"}
                                                     :body (str "{\"k\":\"" (apply str (repeat 2000 "x")) "\"}")})
                  form (tu/fetch! (str base "/echo") {:method :post :body "a=1&b[c]=2" :headers {:content-type "application/x-www-form-urlencoded"}})
                  text (tu/fetch! (str base "/echo") {:method :post :body "hello" :headers {:content-type "text/plain"}})
                  raw (tu/fetch! (str base "/echo") {:method :post :body "bytes" :headers {:content-type "application/octet-stream"}})
                  none (tu/fetch! (str base "/echo") {:method :post :body "x" :headers {:content-type "application/unknown"}})
                  prim (tu/fetch! (str base "/strict-off") {:method :post :body "\"primitive\"" :headers {:content-type "application/vnd.x+json"}})]
            (is (= {:body {:camelKey 1 :x_y [1 2]} :kind "map"} (:json j)) "JSON keys keywordised as spelled, never re-cased")
            (is (= 400 (:status bad)))
            (is (= "parse-failed" (:reason (error-of bad))))
            (is (= 413 (:status big)))
            (is (= "too-large" (:reason (error-of big))))
            (is (= {:body {:a "1" (keyword "b[c]") "2"} :kind "map"} (:json form)) "urlencoded extended=false keeps brackets literal")
            (is (= {:body "hello" :kind "string"} (:json text)))
            (is (= {:body "bytes" :kind "buffer"} (:json raw)))
            (is (= {:body nil :kind "nil"} (:json none)) "unmatched content type skipped silently (pitfall 9.8)")
            (is (= {:body "primitive"} (:json prim)) "per-route parser with strict off and a custom type")))))))

(deftest response-shapes
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/get "/status" (fn [_] {:status 202}))
              (route/get "/end" (fn [_] {:end true}))
              (route/get "/type" (fn [_] {:type "text/csv" :body "a,b"}))
              (route/get "/string" (fn [_] {:body "<b>hi</b>"}))
              (route/get "/vec" (fn [_] {:body [1 {:kw :val}]}))
              (route/get "/num" (fn [_] {:body 42}))
              (route/get "/json-string" (fn [_] {:json true :body "s"}))
              (route/get "/buffer" (fn [_] {:body (tu/buffer-of "bin")}))
              (route/get "/redirect" (fn [_] {:redirect "/status"}))
              (route/get "/redirect301" (fn [_] {:redirect {:url "/status" :status 301}}))
              (route/get "/vary" (fn [_] {:vary [:origin "accept"] :links {:next "/p?page=2" :last "/p?page=9"} :attachment "a b.txt" :body "x"}))
              (route/get "/format" (fn [_] {:format {:json (fn [_] {:body {:fmt "json"}})
                                                     :text (fn [_] {:body "fmt text"})}}))
              (route/get "/format-406" (fn [_] {:format {:json (fn [_] {:body {:fmt "json"}})}}))
              (route/get "/headers-only" (fn [_] {:status 204 :headers {:x-a "1"}})))
        (fn [_ base]
          (p/let [status (tu/fetch! (str base "/status"))
                  end (tu/fetch! (str base "/end"))
                  type (tu/fetch! (str base "/type"))
                  string (tu/fetch! (str base "/string"))
                  vec (tu/fetch! (str base "/vec"))
                  num (tu/fetch! (str base "/num"))
                  js (tu/fetch! (str base "/json-string"))
                  buffer (tu/fetch! (str base "/buffer"))
                  redirect (tu/fetch! (str base "/redirect"))
                  redirect301 (tu/fetch! (str base "/redirect301"))
                  vary (tu/fetch! (str base "/vary"))
                  fj (tu/fetch! (str base "/format") {:headers {:accept "application/json"}})
                  ft (tu/fetch! (str base "/format") {:headers {:accept "text/plain"}})
                  f406 (tu/fetch! (str base "/format-406") {:headers {:accept "image/png"}})
                  ho (tu/fetch! (str base "/headers-only"))]
            (is (= [202 ""] [(:status status) (:body status)]) "status without body -> empty body")
            (is (= 204 (:status end)))
            (is (= "text/csv; charset=utf-8" (get-in type [:headers :content-type])))
            (is (str/starts-with? (get-in string [:headers :content-type]) "text/html") "Express sends strings as HTML unless :type is set")
            (is (= [1 {:kw "val"}] (:json vec)))
            (is (= 42 (:json num)) "numbers render as JSON (pitfall 9.23)")
            (is (= "s" (:json js)) ":json true forces res.json for a string")
            (is (= ["bin" "application/octet-stream"] [(:body buffer) (get-in buffer [:headers :content-type])]))
            (is (= [302 "/status"] [(:status redirect) (get-in redirect [:headers :location])]))
            (is (= 301 (:status redirect301)))
            (is (= "origin, accept" (get-in vary [:headers :vary])) "vary values pass through as given")
            (is (= "</p?page=2>; rel=\"next\", </p?page=9>; rel=\"last\"" (get-in vary [:headers :link])))
            (is (str/starts-with? (get-in vary [:headers :content-disposition]) "attachment") "attachment sets Content-Disposition")
            (is (= {:fmt "json"} (:json fj)))
            (is (= "fmt text" (:body ft)))
            (is (= 406 (:status f406)) "format without a match and no :default -> 406 through the error chain")
            (is (= [204 "1"] [(:status ho) (get-in ho [:headers :x-a])]))))))))

(deftest cookies-round-trip-and-set-cookie-multiplicity
  (async done
    (tu/finish done
      (tu/with-server
        (core/app {:env :test :cookie-secret "top-secret"}
                  (route/get "/set" (fn [_] {:cookies [{:name "a" :value "1" :http-only? true :same-site :lax :max-age 60000 :path "/"}
                                                       {:name "sid" :value "user-9" :signed? true}
                                                       {:name "obj" :value {:k [1]}}]
                                             :clear-cookies [{:name "gone"}]
                                             :body "ok"}))
                  (route/get "/read" (fn [{:keys [cookies signed-cookies]}] {:body {:cookies cookies :signed signed-cookies}})))
        (fn [_ base]
          (p/let [set (tu/fetch! (str base "/set"))
                  cookies (:set-cookies set)
                  a (some #(when (str/starts-with? % "a=") %) cookies)
                  sid (some #(when (str/starts-with? % "sid=") %) cookies)
                  obj (some #(when (str/starts-with? % "obj=") %) cookies)
                  gone (some #(when (str/starts-with? % "gone=") %) cookies)
                  pair (fn [c] (first (str/split c #";")))
                  read (tu/fetch! (str base "/read") {:headers {:cookie (str/join "; " [(pair a) (pair sid) (pair obj) "s2=s:tampered.sig"])}})]
            (is (= 4 (count cookies)) "getSetCookie sees every Set-Cookie (uncertainty 10.4)")
            (is (str/includes? a "HttpOnly"))
            (is (str/includes? a "SameSite=Lax"))
            (is (str/includes? a "Max-Age=60"))
            (is (str/includes? a "Expires="))
            (is (str/includes? sid "sid=s%3A") "signed cookie carries the s: prefix (URI-encoded)")
            (is (str/starts-with? obj "obj=j%3A") "object values serialised as j:<json>")
            (is (str/includes? gone "Expires=Thu, 01 Jan 1970"))
            (is (= {:cookies {:a "1" :obj {:k [1]}} :signed {:sid "user-9" :s2 false}} (:json read))
                "signed cookies verified with Node crypto; tampered -> false")))))))

(deftest signed-cookie-without-secret-is-a-server-error
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/get "/set" (fn [_] {:cookies [{:name "sid" :value "x" :signed? true}] :body "ok"})))
        (fn [_ base]
          (p/let [r (tu/fetch! (str base "/set"))]
            (is (= 400 (:status r)) "argument-error from the response phase renders as 400")
            (is (str/includes? (:message (error-of r)) ":cookie-secret"))))))))

(deftest static-files-with-traversal-guard
  (async done
    (tu/finish done
      (let [dir (tu/temp-dir {"index.html" "<h1>index</h1>" "a/b.txt" "bee" ".secret" "shh" "sub/index.html" "sub"})]
        (tu/with-server
          (app* (route/static "/static" dir {:max-age "1d"})
                {:use [["/allow" (mw/static dir {:dotfiles :allow :index false})]]})
          (fn [_ base]
            (p/let [file (tu/fetch! (str base "/static/a/b.txt"))
                    index (tu/fetch! (str base "/static/"))
                    dot (tu/fetch! (str base "/static/.secret"))
                    dot-allowed (tu/fetch! (str base "/allow/.secret"))
                    traversal (tu/fetch! (str base "/static/..%2f..%2fetc%2fpasswd"))
                    traversal2 (tu/fetch! (str base "/static/a/../../../../etc/passwd"))
                    missing (tu/fetch! (str base "/static/nope.txt"))
                    dir-redirect (tu/fetch! (str base "/static/sub"))
                    no-index (tu/fetch! (str base "/allow/"))]
              (is (= ["bee" "text/plain; charset=utf-8"] [(:body file) (get-in file [:headers :content-type])]))
              (is (= "public, max-age=86400" (get-in file [:headers :cache-control])))
              (is (some? (get-in file [:headers :etag])))
              (is (= "<h1>index</h1>" (:body index)) "index.html served for the directory")
              (is (= 404 (:status dot)) "dotfiles ignored by default")
              (is (= "shh" (:body dot-allowed)) ":dotfiles :allow")
              (is (= 404 (:status traversal)) "encoded traversal refused")
              (is (= 404 (:status traversal2)) "dot-dot segments normalised by the client / refused by send")
              (is (= 404 (:status missing)))
              (is (= "not-found" (:reason (error-of missing))) "static miss falls through to the wrapper 404")
              (is (= 301 (:status dir-redirect)) "directory without trailing slash redirects")
              (is (= 404 (:status no-index)) ":index false"))))))))

(deftest send-file-and-download
  (async done
    (tu/finish done
      (let [dir (tu/temp-dir {"doc.json" "{\"doc\":true}" "inner/x.txt" "xx"})]
        (tu/with-server
          (app* (route/get "/file" (fn [_] {:send-file {:path "doc.json" :root dir}}))
                (route/get "/traverse" (fn [_] {:send-file {:path "../../../etc/passwd" :root dir}}))
                (route/get "/missing" (fn [_] {:send-file {:path "nope" :root dir}}))
                (route/get "/dl" (fn [_] {:download {:path "inner/x.txt" :root dir :filename "renamed.txt"}}))
                (route/get "/relative" (fn [_] {:send-file {:path "doc.json"}})))
          (fn [_ base]
            (p/let [file (tu/fetch! (str base "/file"))
                    traverse (tu/fetch! (str base "/traverse"))
                    missing (tu/fetch! (str base "/missing"))
                    dl (tu/fetch! (str base "/dl"))
                    relative (tu/fetch! (str base "/relative"))]
              (is (= {:doc true} (:json file)))
              (is (= 403 (:status traverse)) "traversal above :root is a 403 (pitfall 9.12)")
              (is (= 404 (:status missing)))
              (is (= "xx" (:body dl)))
              (is (str/includes? (get-in dl [:headers :content-disposition]) "renamed.txt"))
              (is (= 500 (:status relative)) "relative path without :root is a TypeError upstream -> 500"))))))))

(deftest error-chain-sync-throw-rejection-and-handlers
  (async done
    (tu/finish done
      (let [events (atom [])
            on-error-calls (atom [])]
        (tu/with-server
          (core/app {:env :test :on-error (fn [e req] (swap! on-error-calls conj [(util/error-status e) (:path req)]))}
                    (route/get "/sync" (fn [_] (throw (ex-info "sync-boom" {}))))
                    (route/get "/async" (fn [_] (p/rejected (ex-info "async-boom" {}))))
                    (route/get "/async-throw" (fn [_] (p/let [_ (tu/sleep 1)] (throw (tu/js-error "late-boom")))))
                    (route/get "/string" (fn [_] (throw "string-reason")))
                    (route/get "/http" (fn [_] (throw (core/http-error 418 "short and stout" {:headers {:x-tea "yes"} :reason :teapot}))))
                    (route/get "/http-5xx" (fn [_] (throw (core/http-error 503 "db down"))))
                    (route/get "/foreign-status" (fn [_] (throw (ex-info "conflict" {:status 409}))))
                    (route/get "/bad-response" (fn [_] {:body "a" :redirect "/b"}))
                    (route/router "/handled"
                      (route/get "/x" (fn [_] (throw (ex-info "inner" {}))))
                      (route/get "/pass" (fn [_] (throw (ex-info "passed" {}))))
                      (route/error (fn [err req]
                                     (when (= "inner" (ex-message err))
                                       {:status 599 :body {:handled (ex-message err) :path (:path req)}}))))
                    (route/get "/error-in-error" (fn [_] (throw (ex-info "first" {}))))
                    (route/error "/error-in-error" (fn [_ _] (throw (ex-info "second-from-error-mw" {})))))
          (fn [server base]
            (p/let [sync (tu/fetch! (str base "/sync"))
                    async (tu/fetch! (str base "/async"))
                    async-throw (tu/fetch! (str base "/async-throw"))
                    string (tu/fetch! (str base "/string"))
                    http (tu/fetch! (str base "/http"))
                    http5 (tu/fetch! (str base "/http-5xx"))
                    foreign (tu/fetch! (str base "/foreign-status"))
                    bad (tu/fetch! (str base "/bad-response"))
                    handled (tu/fetch! (str base "/handled/x"))
                    passed (tu/fetch! (str base "/handled/pass"))
                    eie (tu/fetch! (str base "/error-in-error"))]
              (is (= [500 {:status 500 :message "sync-boom"}] [(:status sync) (error-of sync)]) "sync throw -> clean 500 with the message (env test)")
              (is (= [500 "async-boom"] [(:status async) (:message (error-of async))]) "promesa rejection -> same path")
              (is (= [500 "late-boom"] [(:status async-throw) (:message (error-of async-throw))]) "throw after an await")
              (is (= [500 "string-reason"] [(:status string) (:message (error-of string))]) "string rejection normalised")
              (is (= [418 {:status 418 :message "short and stout" :reason "teapot"} "yes"]
                     [(:status http) (error-of http) (get-in http [:headers :x-tea])]))
              (is (= [503 "Service Unavailable"] [(:status http5) (:message (error-of http5))]) "5xx http-error message hidden")
              (is (= [409 "conflict"] [(:status foreign) (:message (error-of foreign))]) "foreign :status honoured")
              (is (= 500 (:status bad)) "malformed response map -> 500")
              (is (str/includes? (:message (error-of bad)) "more than one body key"))
              (is (= [599 {:handled "inner" :path "/x"}] [(:status handled) (:json handled)]) "router-level error handler answers")
              (is (= [500 "passed"] [(:status passed) (:message (error-of passed))]) "nil from an error handler passes the error on")
              (is (= [500 "second-from-error-mw"] [(:status eie) (:message (error-of eie))]) "a throw inside an error handler replaces the error")
              (loop [] (when-let [ev (async/poll! (core/events server))] (swap! events conj ev) (recur)))
              (let [errs (filter #(= :error (:event %)) @events)]
                (is (= 10 (count errs)) "every error rendered by the wrapper is emitted (the router-handled one is not)")
                (is (= [500 "/sync"] [(:status (first errs)) (:path (first errs))])))
              (is (= 10 (count @on-error-calls)) ":on-error called per rendered error")
              (is (= [418 "/http"] (nth @on-error-calls 4))))))))))

(deftest production-hides-messages-and-app-level-error-handler
  (async done
    (tu/finish done
      (p/do
        (tu/with-server
          (core/app {:env :production :on-error (fn [_ _] nil)}
                    (route/get "/boom" (fn [_] (throw (ex-info "secret detail" {})))))
          (fn [_ base]
            (p/let [r (tu/fetch! (str base "/boom"))]
              (is (= [500 "Internal Server Error"] [(:status r) (:message (error-of r))]) "production never shows foreign messages"))))
        (tu/with-server
          (core/app {:env :test
                     :error-handler (fn [err _] (when (= 418 (util/error-status err)) {:status 200 :body {:rescued true}}))}
                    (route/get "/tea" (fn [_] (throw (core/http-error 418))))
                    (route/get "/other" (fn [_] (throw (core/http-error 410)))))
          (fn [_ base]
            (p/let [tea (tu/fetch! (str base "/tea"))
                    other (tu/fetch! (str base "/other"))]
              (is (= {:rescued true} (:json tea)) "app-level :error-handler runs before the renderer")
              (is (= 410 (:status other)) "and passes on what it does not handle"))))))))

(deftest trust-proxy-ip-and-ips
  (async done
    (tu/finish done
      (let [h (fn [req] {:body (select-keys req [:ip :ips :protocol :secure? :hostname :host :subdomains])})
            fwd {:x-forwarded-for "10.0.0.9, 10.0.0.8" :x-forwarded-proto "https" :x-forwarded-host "api.example.com"}]
        (p/do
          (tu/with-server (core/app {:env :test :trust-proxy :loopback} (route/get "/" h))
            (fn [_ base]
              (p/let [r (tu/fetch! base {:headers fwd})]
                (is (= {:ip "10.0.0.8" :ips ["10.0.0.8"] :protocol "https" :secure? true :hostname "api.example.com"
                        :host "api.example.com" :subdomains ["api"]}
                       (:json r))
                    "loopback trusted: nearest untrusted hop, forwarded proto/host honoured"))))
          (tu/with-server (core/app {:env :test :trust-proxy 2} (route/get "/" h))
            (fn [_ base]
              (p/let [r (tu/fetch! base {:headers fwd})]
                (is (= "10.0.0.9" (:ip (:json r))) "hop count 2: two proxies trusted")
                (is (= ["10.0.0.9" "10.0.0.8"] (:ips (:json r))) "ips in header order (client first)"))))
          (tu/with-server (app* (route/get "/" h))
            (fn [_ base]
              (p/let [r (tu/fetch! base {:headers fwd})]
                (is (= {:ip "127.0.0.1" :ips [] :protocol "http" :secure? false} (select-keys (:json r) [:ip :ips :protocol :secure?]))
                    "trust proxy off: X-Forwarded-* ignored")))))))))

(deftest streaming-body-readable
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/get "/stream" (fn [_] {:body (core/readable-from ["chunk-1 " "chunk-2 " "chunk-3"])}))
              (route/get "/stream-text" (fn [_] {:status 201 :type :text :headers {:x-s "1"} :body (core/readable-from ["a" (tu/buffer-of "b")])})))
        (fn [_ base]
          (p/let [r (tu/fetch! (str base "/stream"))
                  t (tu/fetch! (str base "/stream-text"))]
            (is (= "chunk-1 chunk-2 chunk-3" (:body r)))
            (is (= "application/octet-stream" (get-in r [:headers :content-type])) "default type for a stream")
            (is (= "chunked" (get-in r [:headers :transfer-encoding])))
            (is (= [201 "ab" "1"] [(:status t) (:body t) (get-in t [:headers :x-s])]))
            (is (str/starts-with? (get-in t [:headers :content-type]) "text/plain"))))))))

(deftest request-helpers-accepts-is-range-with-raw
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/post "/h" (fn [req]
                                 {:body {:accepts (core/accepts? req :json "html")
                                         :accepts-none (core/accepts? req "image/png")
                                         :is (core/is? req :json)
                                         :is-not (core/is? req "text/*")
                                         :range (core/range req 1000)
                                         :raw-method (core/with-raw req (fn [raw-req _res] (:method (core/req->map (core/app {:env :test}) raw-req nil))))}}))
              (route/get "/g" (fn [req] {:body {:is (core/is? req :json) :range (core/range req 10) :bad (core/range req 10)}})))
        (fn [_ base]
          (p/let [r (tu/fetch! (str base "/h") {:method :post :body {:a 1}
                                                :headers {:accept "text/html;q=0.8, application/json" :range "bytes=0-99,200-299"}})
                  g (tu/fetch! (str base "/g") {:headers {:content-type "application/json" :range "bytes=50-"}})]
            (is (= {:accepts "json" :accepts-none nil :is "json" :is-not false
                    :range {:type "bytes" :ranges [{:start 0 :end 99} {:start 200 :end 299}]}
                    :raw-method "post"}
                   (:json r)))
            (is (= {:is nil :range "unsatisfiable" :bad "unsatisfiable"} (:json g)) "req.is is nil without a body (pitfall 9.9)")))))))

(deftest upstream-middleware-in-routes-and-mount-upstream
  (async done
    (tu/finish done
      (let [a (app* (route/use (route/upstream (tu/upstream-middleware "x-upstream-data" "2")))
                    (route/get "/" (fn [_] {:body "ok"})))]
        (core/mount-upstream! a "/extra" (tu/upstream-responder "from-upstream"))
        (tu/with-server a
          (fn [_ base]
            (p/let [r (tu/fetch! base)
                    extra (tu/fetch! (str base "/extra/anything"))]
              (is (= ["2" "ok"] [(get-in r [:headers :x-upstream-data]) (:body r)]) "route/upstream runs before the routes")
              (is (= ["2" "from-upstream"] [(get-in extra [:headers :x-upstream-data]) (:body extra)]) "mount-upstream! answers after the routes")
              (let [e (tu/thrown #(core/mount-upstream! a (fn [_ _ n] (n))))]
                (is (= :express-cljs/argument-error (util/error-type e)) "after listen! the final layers are in place")))))))))

(deftest listen-errors-are-server-errors
  (async done
    (tu/finish done
      (tu/with-server (app*)
        (fn [server _]
          (p/let [e (tu/rejection (core/listen! (app*) {:port (:port (core/address server)) :host "127.0.0.1"}))]
            (is (= :express-cljs/server-error (util/error-type e)))
            (is (= :eaddrinuse (:code (ex-data e))))
            (is (= :listen (:op (ex-data e))))))))))

(deftest listen-options-validated
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(core/listen! (app*) {:prot 1})))))
  (is (= :express-cljs/argument-error (util/error-type (tu/thrown #(core/close! {:server nil} {:bogus 1}))))))

(deftest close-choreography-releases-everything
  (async done
    (tu/finish done
      (p/let [a (app* (route/get "/" (fn [_] {:body "ok"})))
              server (core/listen! a {:port 0 :host "127.0.0.1"})
              hook-args (atom nil)
              _ (core/add-close-hook! server (fn [opts] (reset! hook-args opts) (tu/sleep 5)))
              listening-ev (async/poll! (core/events server))
              r (tu/fetch! (tu/base-url server))
              n (core/connections! server)
              t0 (system-time)
              result (core/close! server {:timeout-ms 2000 :ws-close-timeout-ms 123})
              elapsed (- (system-time) t0)
              _ (tu/sleep 20)
              resources (tu/active-resources)
              again (tu/rejection (core/close! server))]
        (is (= :listening (:event listening-ev)))
        (is (= "ok" (:body r)))
        (is (<= 0 n 1) "at most the keep-alive connection is open before close")
        (is (= {:closed true :forced? false} result) "idle keep-alive connections do not block close (Node >= 19)")
        (is (< elapsed 1500) (str "close! resolved in " elapsed " ms"))
        (is (= {:timeout-ms 123} @hook-args) "close hooks receive the ws deadline")
        (is (false? (core/listening? server)))
        (is (nil? (core/address server)))
        (is (not (some #{"TCPServerWrap"} resources)) (str "no server handle left: " (pr-str resources)))
        (is (= :err-server-not-running (:code (ex-data again))) "closing twice rejects")
        (loop [evs []]
          (if-let [ev (async/poll! (core/events server))]
            (recur (conj evs ev))
            (is (some #(= :close (:event %)) evs) "the :close event was emitted")))))))

(deftest close-forces-stuck-requests-after-timeout
  (async done
    (tu/finish done
      (let [never (p/create (fn [_ _] nil))]
        (p/let [a (app* (route/get "/stuck" (fn [_] never)))
                server (core/listen! a {:port 0 :host "127.0.0.1"})]
          ;; NB: the in-flight request is bound with `let`, not `p/let`, which
          ;; would await the pending promise.
          (let [stuck (-> (tu/fetch! (str (tu/base-url server) "/stuck")) (p/catch (fn [_] :rejected)))]
            (p/let [_ (tu/sleep 30)
                    t0 (system-time)
                    result (core/close! server {:timeout-ms 100})
                    elapsed (- (system-time) t0)
                    outcome stuck]
              (is (= {:closed true :forced? true} result) "closeAllConnections fallback fired")
              (is (< elapsed 1500) (str "forced close in " elapsed " ms"))
              (is (= :rejected outcome) "the stuck client saw its connection cut")
              (is (false? (core/listening? server))))))))))

(deftest x-powered-by-and-etag-settings-on-the-wire
  (async done
    (tu/finish done
      (p/do
        (tu/with-server (core/app {:env :test :x-powered-by? true} (route/get "/" (fn [_] {:body "x"})))
          (fn [_ base]
            (p/let [r (tu/fetch! base)]
              (is (= "Express" (get-in r [:headers :x-powered-by])))
              (is (some? (get-in r [:headers :etag])) "weak ETag by default"))))
        (tu/with-server (core/app {:env :test :etag false} (route/get "/" (fn [_] {:body "x"})))
          (fn [_ base]
            (p/let [r (tu/fetch! base)
                    conditional (tu/fetch! base {:headers {:if-none-match "W/\"1-abc\""}})]
              (is (nil? (get-in r [:headers :etag])))
              (is (= 200 (:status conditional))))))))))

(deftest conditional-get-304-with-fresh
  (async done
    (tu/finish done
      (tu/with-server
        (app* (route/get "/doc" (fn [req]
                                  (let [headers {:etag "\"v1\""}]
                                    (if (core/fresh? req headers)
                                      {:status 304 :headers headers}
                                      {:headers headers :body {:v 1}})))))
        (fn [_ base]
          (p/let [first-r (tu/fetch! (str base "/doc"))
                  ;; Node's fetch appends `Cache-Control: no-cache` to conditional
                  ;; requests (Fetch spec, cache mode no-store), which `fresh`
                  ;; treats as a forced reload; sending our own Cache-Control
                  ;; keeps the request a plain conditional GET.
                  cached (tu/fetch! (str base "/doc") {:headers {:if-none-match "\"v1\"" :cache-control "max-age=0"}})
                  reload (tu/fetch! (str base "/doc") {:headers {:if-none-match "\"v1\""}})]
            (is (= [200 "\"v1\""] [(:status first-r) (get-in first-r [:headers :etag])]))
            (is (= [304 ""] [(:status cached) (:body cached)]) "fresh? drives a 304")
            (is (= 200 (:status reload)) "fetch's implicit Cache-Control: no-cache forces a full response")))))))
