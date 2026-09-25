(ns express-cljs.test-util
  "Shared fixtures for the express-cljs tests. Not a test namespace itself
   (the name does not end in -test). Phase 0 provides only the promise /
   throw helpers; Phase 2 adds an ephemeral-port server factory and a
   loopback HTTP client (Node `fetch`), Phase 3 a `ws` client, all confined
   to this namespace so the test files stay interop-free."
  (:require [cljs.test :refer [is]]
            [promesa.core :as p]))

(defn rejection
  "Promise of the rejection reason of `p` (fails the assertion if it resolves)."
  ;; NB: promesa treats a js/Error RETURNED from a catch handler as a
  ;; rejection (IPromiseFactory on js/Error), so the error must be wrapped in
  ;; an already-resolved promise to travel as a value.
  [promise]
  (-> promise
      (p/then (fn [v] (throw (ex-info "expected rejection" {:resolved v}))))
      (p/catch (fn [e] (p/resolved e)))))

(defn thrown
  "The value thrown by (f), nil when it returns normally."
  [f]
  (try (f) nil (catch :default e e)))

(defn finish
  "Runs promise `p` to completion inside an (async done ...) block: an
   unexpected rejection fails the test with its message; `done` is always
   called."
  [done p]
  (-> (p/promise p)
      (p/catch (fn [e]
                 (is false (str "unexpected rejection: " (ex-message e) " " (pr-str (ex-data e))))))
      (p/finally (fn [_ _] (done)))))

(defn sleep
  "Promise resolved after `ms` milliseconds."
  [ms]
  (p/delay ms nil))
