(ns eft.core
  (:refer-clojure :exclude [apply iterate last reductions])
  (:require [clojure.core :as c]))

(defn iterate
  "The classic iterate function, as in core, but with a transducer arity"
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [result (rf result input)]
          (if (reduced? result)
            @result
            (recur result (f input))))))))
  ([f x] (clojure.lang.Iterate/create f x)))


(defn reductions
  ([f] (reductions f (f)))
  ([f init]
   (fn [rf]
     (let [vacc (volatile! init)]
       (fn
         ([] (rf))
         ([result]
          (let [result (unreduced (rf result @vacc))]
            (rf result)))
         ([result input]
          (let [result (rf result @vacc)]
            (if (reduced? result)
              @result
              (let [vacc (vswap! vacc f input)]
                (if (reduced? vacc)
                  (reduced result)
                  result))))))))))

(defn produce
  "Like transduce but for a scalar input rather than a collection.
  you could just call `(transduce f init [x])` or similar of course"
  ([xform f x] (produce xform f (f) x))
  ([xform f init x]
   (let [f (xform f)
         ret (f init x)]
     (f ret))))


;; * useful transducers to use with iterate

(def til-nil
  (take-while (complement nil?)))

(defn- exception?
  [e]
  (instance? Exception e))

(def halt-ex
  (take-while (complement exception?)))


(defn fixed-point
  "Takes a comparison function cmp (uses `=` if none is supplied) and
  takes values until (cmp previous-input input) is true.
  Like a combination of `take-while` and `dedupe`"
  ([] (fixed-point =))
  ([eq-fn]
   (fn [rf]
     (let [pi (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [previous-input @pi]
            (vreset! pi input)
            (if (eq-fn previous-input input)
              (reduced result)
              (rf result input)))))))))

;; * reducing fns

(defn last
  ([] nil)
  ([x] x)
  ([_ x] x))

(defn apply
  [v f]
  (f v))


(comment
  "testing produce"
  (transduce (comp (iterate inc)
                   (take 5))
             conj
             [1])

  (produce (comp (iterate inc)
                 (take 5))
           conj
           1)
  ,)

(comment
  "test fixed-point"

  (defn test-step [input]
    (if (> input 5)
      input
      (inc input)))

  (produce (comp (iterate test-step)
                 (fixed-point))
           conj
           1)
  ,)
(comment
  "reductions example"

  (require '[criterium.core :as criterium])
  
  (criterium/bench

   (into [] (c/reductions apply {:foo {:bar {:baz 42}}} [:foo :bar :baz]))

   ,) 
  ;; Execution time mean : 467.624557 ns ;; 
  ;; Execution time std-deviation : 10.848428 ns

  (criterium/bench

   (transduce (reductions apply {:foo {:bar {:baz 42}}}) conj [] [:foo :bar :baz])

   ,)
  ;; Execution time mean : 136.384626 ns
  ;; Execution time std-deviation : 2.851452 ns

  ,)
