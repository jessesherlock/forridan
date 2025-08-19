(ns forridan.flock-test
  (:require [clojure.test :refer [deftest is]]
            [forridan.flock :as sut]))

(defn deflock
  [flock]
  (loop [fl flock
         acc []]
    (if-let [fl (sut/rotate fl)]
      (recur fl (conj acc (sut/leader fl)))
      acc)))

(deftest seq-flock-test
  (is (= [1 2 3 4 5]
         (deflock (sut/seq-flock [1 2 3 4 5])))))

(deftest vec-flock-test
  (is (= [1 2 3 4 5]
         (deflock (sut/vec-flock [1 2 3 4 5])))))

(deftest tree-flock-test
  (is (= [1 2 3 4 5]
         (deflock (sut/tree-flock [1 2 3 4 5]))))
  (is (= [1 2 3 4 5]
         (deflock
           (sut/tree-flock {:label :root
                            :children [{:label :left, :children [1 2 3]}
                                       {:label :right, :children [4 5]}]})))))
