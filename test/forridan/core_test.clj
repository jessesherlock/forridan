(ns forridan.core-test
  (:require [clojure.test :refer [deftest is]]
            [net.r4s6.test-async :as ta :include-macros true]
            [clojure.core.async :as a]
            [soliton.core :as l]
            [soliton.async :as la]
            [forridan.flock :as flock]
            [forridan.core :as sut]))

(defn ainc [x] (a/go (inc x)))
(def inc-a (l/<> inc :a))
(def inc-b (l/<> inc :b))
(def ainc-a (la/<> ainc :a))

(deftest run-sync-test
  (is (= {:a 1 :b 1}
         (-> (sut/run-sync {:flock (flock/vec-flock [inc-a inc-b])
                            :a 0 :b 0})
             (dissoc :flock)))))

(deftest steps-sync
  (is (= [{:a 0 :b 0}
          {:a 1 :b 0}
          {:a 1 :b 1}]
         (map #(dissoc % :flock)
              (sut/steps-sync {:flock (flock/vec-flock [inc-a inc-b])
                               :a 0 :b 0})))))

(deftest run-test
  (ta/async
      done
      (a/go
        (is (= {:a 1 :b 1}
               (-> (sut/run {:flock (flock/seq-flock [inc-a inc-b])
                             :a 0 :b 0})
                   a/<!
                   (dissoc :flock))
               (-> (sut/run {:flock (flock/seq-flock [ainc-a inc-b])
                             :a 0 :b 0})
                   a/<!
                   (dissoc :flock))))
        (done))))

(deftest steps-test
  (ta/async
      done
      (a/go 
        (is (= [{:a 0 :b 0}
                {:a 1 :b 0}
                {:a 1 :b 1}]
               (->> (sut/steps {:flock (flock/seq-flock [inc-a inc-b])
                                :a 0 :b 0})
                    (a/into [])
                    a/<!
                    (map #(dissoc % :flock)))
               (->> (sut/steps {:flock (flock/seq-flock [ainc-a inc-b])
                                :a 0 :b 0})
                    (a/into [])
                    a/<!
                    (map #(dissoc % :flock)))))
        (done))))
