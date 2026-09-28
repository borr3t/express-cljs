(ns express-cljs.examples-test
  "The examples of the README, run. Nothing leaves the machine: every
   example of dev/express_cljs/examples.cljs starts its server on a free
   port of the loopback interface, talks to it and closes it.

   The README is held to the code:
   - what stands between a README:begin and a README:end mark of
     dev/express_cljs/examples.cljs stands in README.md as written;
   - every one-liner of `shown` stands in README.md with the value it
     evaluates to;
   - every result README.md prints is the result the example returns.
   Spacing and commas are not compared."
  (:require ["fs" :as fs]
            [cljs.test :refer [deftest is testing async]]
            [clojure.string :as str]
            [express-cljs.core :as core]
            [express-cljs.examples :as ex]
            [express-cljs.route :as route]
            [express-cljs.rpc :as rpc]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [promesa.core :as p])
  (:require-macros [express-cljs.doctest :refer [claims]]))

;; ---------------------------------------------------------------------------
;; The one-liners
;; ---------------------------------------------------------------------------

(defn handler
  "A handler, for the one-liners that need one."
  [_req]
  {:body "ok"})

(def shown
  "The one-liners of the README, each with its value."
  (claims
   (route/plan-paths (route/compile {:get {"/files/*path" handler "/users/:id" handler "/users/me" handler}}))
   (core/parse-cookies "sid=abc; theme=dark")
   (core/cookie-sign "abc" "a secret of yours")
   (core/cookie-unsign "abc.4DZonNBp9SYRcW/5DU2TZmU041qknXxCRgjR5IZ6QjQ" "a secret of yours")
   (core/fresh? {:headers {:if-none-match "\"v1\""}} {:etag "\"v1\""})
   (util/->wire-headers {:content-type "text/plain" :x-trace nil})
   (util/error-status (core/http-error 404 "no such user"))
   (util/error-status (rpc/error :timeout "too slow"))
   (rpc/request-frame 1 "math/add" {:a 1 :b 2})
   (rpc/error-frame 1 :method-not-found "method not found: nope")
   (ws/valid-close-code? 4001)
   (ws/valid-close-code? 1006)))

;; ---------------------------------------------------------------------------
;; Reading the README
;; ---------------------------------------------------------------------------

(defn- slurp [path]
  (fs/readFileSync path "utf8"))

(defn- plain
  "`text` without what is not compared: commas, and any run of whitespace
   as one space."
  [text]
  (-> text
      (str/replace "," "")
      (str/replace #"\s+" " ")
      str/trim))

(defn- region
  "The text between the marks README:begin `label` and README:end `label`
   of the file at `path`, nil when either mark is missing."
  [path label]
  (let [[_ found] (re-find (re-pattern (str "(?s);; README:begin " label "\\n(.*?)\\n\\s*;; README:end " label))
                           (slurp path))]
    found))

(defn- shows?
  "Does README.md show `text`?"
  [text]
  (str/includes? (plain (slurp "README.md")) (plain text)))

(defn- shown-in-readme
  "Asserts README.md prints `value`."
  [value]
  (is (shows? (pr-str value)) (str "README.md does not show:\n" (pr-str value))))

(defn- run
  "Runs the promise of `(f)` as an async test body."
  [done f]
  (-> (f)
      (p/catch (fn [e] (is false (str "unexpected rejection: " (ex-message e) " " (pr-str (ex-data e))))))
      (p/finally (fn [_ _] (done)))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest the-quick-start-answers-over-http
  (async done
    (run done
      (fn []
        (p/let [r (ex/quick-start)]
          (testing "the user is created, and found where the answer says"
            (is (= {:status 201
                    :body {:name "Grace" :id "2"}
                    :headers {:location "/users/2"}}
                   (:created r)))
            (is (= {:status 200 :body {:name "Grace" :id "2"}} (:found r))))
          (testing "the http-error a handler throws is the answer"
            (is (= {:status 404 :body {:error {:status 404 :message "no such user"}}}
                   (:missing r))))
          (shown-in-readme r))))))

(deftest the-harness-closes-its-server
  (async done
    (run done
      (fn []
        (p/let [seen (atom nil)
                value (ex/with-server (core/app {:env :test})
                        (fn [server _url] (reset! seen server) :done))
                failure (-> (ex/with-server (core/app {:env :test})
                              (fn [server _url] (reset! seen server) (throw (ex-info "failed" {:why :test}))))
                            (p/then (fn [_] nil))
                            (p/catch (fn [e] (ex-data e))))]
          (is (= :done value))
          (is (= {:why :test} failure) "the failure of f is the failure of with-server")
          (is (false? (core/listening? @seen)) "closed after a failure too"))))))

(deftest routes-are-guarded
  (async done
    (run done
      (fn []
        (p/let [r (ex/guard)]
          (testing "no token: 401, with the challenge"
            (is (= 401 (get-in r [:anonymous :status])))
            (is (= "Bearer realm=\"api\"" (get-in r [:anonymous :headers :www-authenticate]))))
          (testing "the entry of the token reaches the handler"
            (is (= {:status 200 :body {:user "bob"}} (:bob r))))
          (testing "the mounted router answers under its mount"
            (is (= 403 (get-in r [:bob-admin :status])))
            (is (= {:status 200 :body {:users 2}} (:ada-admin r))))
          (testing "a resource answers the methods it has not with 405 and Allow"
            (is (= 405 (get-in r [:deleted :status])))
            (is (= "GET, HEAD" (get-in r [:deleted :headers :allow]))))
          (shown-in-readme r))))))

(deftest a-chat-is-held
  (async done
    (run done
      (fn []
        (p/let [r (ex/chat)]
          (is (= {:welcome {:welcome "ada"}
                  :bob-hears {:from "ada" :text "hello"}
                  :nameless {:type :express-cljs/ws-error :code :unexpected-response :status 401}}
                 r))
          (shown-in-readme r))))))

(deftest one-registry-serves-two-transports
  (async done
    (run done
      (fn []
        (p/let [r (ex/rpc)]
          (testing "the same method, the same answer"
            (is (= {:status 200 :body {:jsonrpc "2.0" :id 1 :result 3}} (:over-http r)))
            (is (= 3 (:over-ws r))))
          (testing "a refusal of the method is an error frame, and HTTP 200"
            (is (= 200 (get-in r [:refused :status])))
            (is (= -32602 (get-in r [:refused :body :error :code]))))
          (testing "progress arrives as notifications that name the call, then the result"
            (is (= [{:id 2 :step 1} {:id 2 :step 2}] (:progress r)))
            (is (= {:done 2} (:job r))))
          (shown-in-readme r))))))

(deftest failures-are-data
  (async done
    (run done
      (fn []
        (p/let [r (ex/errors)]
          (is (= {:port-taken {:ok? false :why :server :code :eaddrinuse}
                  :no-socket {:ok? false :why :websocket :code :unexpected-response :status 404}
                  :bad-params {:ok? false :why :rpc :code :invalid-params}
                  :bad-route {:ok? false :why :argument}
                  :added {:ok? true :value 3}}
                 r))
          (shown-in-readme r))))))

(deftest what-is-not-of-the-library-is-rethrown
  (async done
    (run done
      (fn []
        (p/let [e (-> (ex/outcome (p/rejected (ex-info "foreign" {:type :other})))
                      (p/then (fn [_] nil))
                      (p/catch (fn [e] (ex-data e))))]
          (is (= {:type :other} e)))))))

(deftest the-one-liners-hold
  (let [value (into {} shown)
        signed (value "(core/cookie-sign \"abc\" \"a secret of yours\")")]
    (is (= (count shown) (count value)) "no one-liner is shown twice")
    (is (= [[:get "/users/me"] [:get "/users/:id"] [:get "/files/*path"]]
           (second (first shown)))
        "literal before parameter before wildcard")
    (is (= {:sid "abc" :theme "dark"} (value "(core/parse-cookies \"sid=abc; theme=dark\")")))
    (is (str/starts-with? signed "abc."))
    (is (= "abc" (core/cookie-unsign signed "a secret of yours")))
    (is (false? (core/cookie-unsign signed "another secret")))
    (is (= [404 504] (mapv second (filter #(str/includes? (first %) "error-status") shown))))
    (is (= [true false] (mapv second (filter #(str/includes? (first %) "valid-close-code?") shown))))))

(deftest the-readme-shows-what-runs
  (testing "the marked code of the examples"
    (doseq [label ["harness" "quick-start" "guard" "chat" "rpc" "errors"]
            :let [code (region "dev/express_cljs/examples.cljs" label)]]
      (is (some? code) (str "no region " label))
      (is (shows? code) (str "README.md does not show " label ":\n" code))))
  (testing "every one-liner, with its value"
    (doseq [[source value] shown
            :let [line (str source "\n;; => " (pr-str value))]]
      (is (shows? line) (str "README.md does not show:\n" line))))
  (testing "how the examples are run, and where the manual is"
    (is (shows? "npx shadow-cljs compile examples && node target/examples.js"))
    (is (shows? "docs/express-cljs.org"))
    (is (shows? "docs/express-api-inventory.org"))))
