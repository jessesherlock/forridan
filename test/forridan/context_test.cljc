(ns forridan.context-test
  (:require [clojure.test :refer [deftest is]]
            [forridan.context :as sut]
            [forridan.core :as core]
            [soliton.core :as l]))

(deftest decontext-test
  (is (= {:val 0}
         (sut/decontext (sut/flock-context [inc inc] {:val 0}))
         (sut/decontext (sut/simple-context inc {:val 0})))))

(defn test-run
  ([c] (sut/decontext (core/run-sync c)))
  ([stepfn c] (sut/decontext (core/run-sync stepfn c))))

(def inc-a (l/<> inc :a))
(def inc-b (l/<> inc :b))
(def inc-c (l/<> inc :c))

(deftest flock-context-test
  (is (= {:a 1 :b 1 :c 1}
         (test-run (sut/flock-context
                    [inc-a inc-b inc-c]
                    {:a 0 :b 0 :c 0}))))
  (is (= {:a 1 :b 1 :c 1}
         (test-run (sut/flock-context
                    (seq [inc-a inc-b inc-c])
                    {:a 0 :b 0 :c 0}))))
  (is (= {:a 1 :b 1 :c 1}
         (test-run (sut/flock-context
                    {:label :inc-fns :children [inc-a inc-b inc-c]}
                    {:a 0 :b 0 :c 0}))))
  (is (= {:a 1 :b 1 :c 1}
         (test-run (sut/flock-context
                    (list #(assoc % :a 0 :b 0 :c 0)
                          inc-a inc-b inc-c)
                    {:a 0 :b 0 :c 0})))))

(deftest sm-context-test
  (let [undervalue (l/<> inc :val)
        at-value (l/<> str :val :as-str)
        complete (constantly nil)
        tf (fn [c] (cond
                     (< (:val c) 5) undervalue
                     (:as-str c) complete
                     (= (:val c) 5) at-value))]
    (is (= {:val 5 :as-str "5"}
           (test-run sut/sm-step (sut/sm-context tf {:val 0}))))))

(deftest simple-step-test
  (let [step-fn (fn [c] (if (< (:val c) 5)
                          (update c :val inc)))]
    (is (= {:val 5}
           (test-run sut/simple-step (sut/simple-context step-fn {:val 0}))))))
