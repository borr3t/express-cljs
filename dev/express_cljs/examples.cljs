(ns express-cljs.examples
  "The examples of the README, runnable OFFLINE:

     npx shadow-cljs compile examples && node target/examples.js

   Every example starts a real server on a free port of the loopback
   interface (127.0.0.1), talks to it over HTTP or a websocket, and closes
   it: nothing leaves the machine. What stands behind the handlers is a
   stub — an atom of users, a map of tokens.

   What stands between a README:begin and a README:end mark stands in
   README.md as written (test/express_cljs/examples_test.cljs holds the
   README to it and runs every function of this file)."
  (:refer-clojure :exclude [methods])
  (:require [cljs.core.async :as async :refer [<! go-loop]]
            [cljs.pprint :as pprint]
            [clojure.string :as str]
            [express-cljs.core :as core]
            [express-cljs.middleware :as mw]
            [express-cljs.rpc :as rpc]
            [express-cljs.util :as util]
            [express-cljs.ws :as ws]
            [promesa.core :as p]))

;; ---------------------------------------------------------------------------
;; The harness: a server on the loopback interface, and a client for it
;; ---------------------------------------------------------------------------

;; README:begin harness
(defn with-server
  "Listens `app` on a free port of the loopback interface, runs
   `(f server url)` and closes the server, whatever `f` did. Resolves
   with what `f` resolved with."
  [app f]
  (p/let [server  (core/listen! app {:port 0 :host "127.0.0.1"})
          url     (str "http://127.0.0.1:" (:port (core/address server)))
          outcome (-> (p/do (f server url))
                      (p/then (fn [value] {:value value}))
                      (p/catch (fn [e] {:error e})))
          _       (core/close! server {:timeout-ms 1000})]
    (if-let [e (:error outcome)]
      (throw e)
      (:value outcome))))

(defn request!
  "One HTTP request, made with Node's fetch. `opts`: :method (default
   :get), :headers (a map of kebab keywords), :body (a map, sent as JSON),
   :show (the response headers to report). Resolves with {:status :body},
   and :headers when :show is given; a JSON body arrives parsed."
  ([url] (request! url {}))
  ([url {:keys [method headers body show]}]
   (p/let [init {:method  (str/upper-case (name (or method :get)))
                 :headers (cond-> (util/->wire-headers (or headers {}))
                            body (assoc "content-type" "application/json"))}
           init (cond-> init
                  body (assoc :body (js/JSON.stringify (clj->js body))))
           resp (js/fetch url (clj->js init))
           text (.text resp)
           json? (str/includes? (str (.get (.-headers resp) "content-type")) "json")]
     (cond-> {:status (.-status resp)
              :body   (if json?
                        (js->clj (js/JSON.parse text) :keywordize-keys true)
                        text)}
       show (assoc :headers (into {} (map (fn [h] [h (.get (.-headers resp) (name h))])) show))))))
;; README:end harness

;; ---------------------------------------------------------------------------
;; Quick start: an HTTP API over a stub
;; ---------------------------------------------------------------------------

;; README:begin quick-start
(defn api
  "An HTTP API over `users`, an atom of {id user}: the stub that stands
   where a database would."
  [users]
  (core/app
   {:env :production :json true}
   {:get  {"/users/:id" (fn [{:keys [params]}]
                          (if-let [user (get @users (:id params))]
                            {:body user}
                            (throw (core/http-error 404 "no such user"))))}
    :post {"/users"     (fn [{:keys [body]}]
                          (let [id   (str (inc (count @users)))
                                user (assoc body :id id)]
                            (swap! users assoc id user)
                            {:status  201
                             :headers {:location (str "/users/" id)}
                             :body    user}))}}))

(defn quick-start []
  (with-server (api (atom {"1" {:id "1" :name "Ada"}}))
    (fn [_server url]
      (p/let [created (request! (str url "/users") {:method :post
                                                    :body   {:name "Grace"}
                                                    :show   [:location]})
              found   (request! (str url "/users/2"))
              missing (request! (str url "/users/9"))]
        {:created created
         :found   found
         :missing missing}))))
;; README:end quick-start

;; ---------------------------------------------------------------------------
;; Scenario 1: routes as data, guarded by middleware
;; ---------------------------------------------------------------------------

;; README:begin guard
(def tokens
  "The stub that stands where a token store would."
  {"token-of-ada" {:user "ada" :admin? true}
   "token-of-bob" {:user "bob"}})

(defn signed-in
  "`handler`, for requests that carry a bearer token of `tokens`; the
   entry of the token reaches the handler as :auth."
  [handler]
  (mw/wrap-bearer-auth handler {:verify (fn [token] (get tokens token))}))

(defn admins-only [handler]
  (fn [{:keys [auth] :as req}]
    (if (:admin? auth)
      (handler req)
      (throw (core/http-error 403 "admins only")))))

(def guarded-api
  {:use      [(mw/request-id)]
   :get      {"/me" (signed-in (fn [{:keys [auth]}] {:body auth}))}
   :routers  [{:mount  "/admin"
               :routes {:get {"/stats" (signed-in (admins-only (fn [_] {:body {:users 2}})))}}}]
   :resource {"/health" {:get (fn [_] {:body {:ok true}})}}})

(defn guard []
  (with-server (core/app {:env :production} guarded-api)
    (fn [_server url]
      (let [as (fn [token] {:headers {:authorization (str "Bearer " token)}})]
        (p/let [anonymous (request! (str url "/me") {:show [:www-authenticate]})
                bob       (request! (str url "/me") (as "token-of-bob"))
                bob-admin (request! (str url "/admin/stats") (as "token-of-bob"))
                ada-admin (request! (str url "/admin/stats") (as "token-of-ada"))
                deleted   (request! (str url "/health") {:method :delete :show [:allow]})]
          {:anonymous anonymous
           :bob       bob
           :bob-admin bob-admin
           :ada-admin ada-admin
           :deleted   deleted})))))
;; README:end guard

;; ---------------------------------------------------------------------------
;; Scenario 2: a chat over websockets
;; ---------------------------------------------------------------------------

;; README:begin chat
(defn serve-chat!
  "A chat at /chat of `server`. A client names itself in the query of the
   handshake; without a name the handshake is refused with HTTP 401. What
   a client says goes to everyone else in the lobby."
  [server]
  (let [chat (ws/serve! server {:path "/chat"
                                :authenticate (fn [{:keys [query]}]
                                                (when-let [name (:name query)]
                                                  {:name name}))})]
    (ws/on-open! chat
                 (fn [conn]
                   (let [name (:name (ws/ctx conn))]
                     (ws/join! chat conn "lobby")
                     (ws/send-json! conn {:welcome name})
                     (go-loop []
                       (when-let [message (<! (ws/messages conn))]
                         (ws/emit! chat "lobby"
                                   {:json {:from name :text (:data message)}}
                                   {:except conn})
                         (recur))))))
    chat))

(defn heard
  "Promise of the next message of `conn`, parsed."
  [conn]
  (p/create (fn [resolve _]
              (async/take! (ws/messages conn)
                           (fn [message] (resolve (some-> message ws/json)))))))

(defn chat []
  (with-server (core/app {:env :production})
    (fn [server url]
      (let [chat-url (str (str/replace url "http://" "ws://") "/chat")]
        (serve-chat! server)
        (p/let [ada      (ws/connect! (str chat-url "?name=ada"))
                welcome  (heard ada)
                bob      (ws/connect! (str chat-url "?name=bob"))
                _        (heard bob)
                _        (ws/send! ada "hello")
                bob-hears (heard bob)
                nameless (-> (ws/connect! chat-url)
                             (p/then (fn [_] :accepted))
                             (p/catch (fn [e] (select-keys (ex-data e) [:type :code :status]))))]
          {:welcome   welcome
           :bob-hears bob-hears
           :nameless  nameless})))))
;; README:end chat

;; ---------------------------------------------------------------------------
;; Scenario 3: one RPC registry over HTTP and over a websocket
;; ---------------------------------------------------------------------------

;; README:begin rpc
(def methods
  (rpc/registry
   {"math/add" (fn [{:keys [a b]} _ctx]
                 (when-not (and (number? a) (number? b))
                   (throw (rpc/error :invalid-params "a and b are numbers")))
                 (+ a b))
    "jobs/run" (fn [{:keys [steps]} {:keys [notify! id]}]
                 ;; notify! is nil over HTTP: progress needs a connection
                 (p/loop [step 1]
                   (if (> step steps)
                     {:done steps}
                     (p/let [_ (when notify!
                                 (notify! "jobs/progress" {:id id :step step}))]
                       (p/recur (inc step))))))}))

(defn rpc []
  (with-server (core/app {:env :production} (rpc/http-routes methods))
    (fn [server url]
      (rpc/serve-ws! (ws/serve! server {:path "/rpc"}) methods)
      (let [post   (fn [frame] (request! (str url "/rpc") {:method :post :body frame}))
            ws-url (str (str/replace url "http://" "ws://") "/rpc")]
        (p/let [over-http (post {:jsonrpc "2.0" :id 1 :method "math/add" :params {:a 1 :b 2}})
                refused   (post {:jsonrpc "2.0" :id 2 :method "math/add" :params {:a 1}})
                conn      (ws/connect! ws-url)
                client    (rpc/client conn)
                call!     (:call! client)
                over-ws   (call! "math/add" {:a 1 :b 2})
                job       (call! "jobs/run" {:steps 2})
                progress  (p/all (repeatedly 2 #(p/create
                                                 (fn [resolve _]
                                                   (async/take! (:notifications client) resolve)))))]
          {:over-http over-http
           :refused   refused
           :over-ws   over-ws
           :progress  (mapv :params progress)
           :job       job})))))
;; README:end rpc

;; ---------------------------------------------------------------------------
;; Error handling
;; ---------------------------------------------------------------------------

;; README:begin errors
(defn outcome
  "What became of `promise`: its value, or its failure as data."
  [promise]
  (-> promise
      (p/then (fn [value] {:ok? true :value value}))
      (p/catch (fn [e]
                 (let [{:keys [type code status]} (ex-data e)]
                   (case type
                     :express-cljs/server-error   {:ok? false :why :server :code code}
                     :express-cljs/ws-error       {:ok? false :why :websocket :code code :status status}
                     :express-cljs/rpc-error      {:ok? false :why :rpc :code code}
                     :express-cljs/http-error     {:ok? false :why :http :status status}
                     :express-cljs/argument-error {:ok? false :why :argument}
                     (throw e)))))))

(defn errors []
  (with-server (core/app {:env :production} (rpc/http-routes methods))
    (fn [server url]
      (rpc/serve-ws! (ws/serve! server {:path "/rpc"}) methods)
      (p/let [port       (:port (core/address server))
              conn       (ws/connect! (str "ws://127.0.0.1:" port "/rpc"))
              call!      (:call! (rpc/client conn))
              port-taken (outcome (core/listen! (core/app {}) {:port port :host "127.0.0.1"}))
              no-socket  (outcome (ws/connect! (str "ws://127.0.0.1:" port "/nowhere")))
              bad-params (outcome (call! "math/add" {:a 1}))
              bad-route  (outcome (p/do (core/app {} {:get {"/files/*" (fn [_] {:body "file"})}})))
              added      (outcome (call! "math/add" {:a 1 :b 2}))]
        {:port-taken port-taken
         :no-socket  no-socket
         :bad-params bad-params
         :bad-route  bad-route
         :added      added}))))
;; README:end errors

;; ---------------------------------------------------------------------------
;; Runner
;; ---------------------------------------------------------------------------

(defn- show [title value]
  (println (str ";; " title))
  (pprint/pprint value))

(defn main [& _]
  (-> (p/let [started (quick-start)
              guarded (guard)
              chatted (chat)
              called  (rpc)
              failed  (errors)]
        (show "quick start: an HTTP API over a stub" started)
        (show "routes as data, guarded by middleware" guarded)
        (show "a chat over websockets" chatted)
        (show "one RPC registry, two transports" called)
        (show "failures are data" failed))
      (p/catch (fn [e]
                 (println "example failed:" (ex-message e) (pr-str (ex-data e)))
                 (set! (.-exitCode js/process) 1)))))
