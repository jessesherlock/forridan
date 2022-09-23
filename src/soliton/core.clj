(ns soliton.core
  (:refer-clojure :exclude [first nth second select-keys])
  (:require [clojure.core :as c]
            [eft.core :as eft]))

(comment
  (use 'debux.core)

  ,)

;; * protocols

(defprotocol Focus
  (-focus [l s]))

(defprotocol Over
  (-over [l f s]))

(comment 
  ;; XXX think more on if we want to include put as a core interface
  ;; choosing between over and put is easy, over is more general and needs to be fundamental
  ;; `(constantly v)` is a static fn and easily optimized so is a miniscule perf hit
  ;; might be useful to have the ability to make lenses that take a getter, setter and updater
  (defprotocol Put
    (-put [l v s]))

,)


;; * simple lenses

(extend clojure.lang.IFn
  Focus
  {:-focus (fn [l v] (l v))}
  Over
  {:-over (fn [l f s] (l s f))})

(extend clojure.lang.Keyword
  Focus
  {:-focus (fn [l v] (get v l))}
  Over
  {:-over (fn [l f s] (update s l f))})

(extend java.lang.Long
  Focus
  {:-focus (fn [l v] (c/nth v l))}
  Over
  {:-over (fn [l f s] (update s l f))})


;; * core fns

(defn focus
  [l s]
  (-focus l s))

(defn put
  [l v s]
  (-over l (constantly v) s))

(defn over
  [l f s]
  (-over l f s))

;; * util fns

(defn focus-rf
  [s l]
  (-focus l s))

(defn coll-lens-focus
  [ls s]
  (reduce focus-rf s ls))

(defn coll-lens-focii
  [ls s]
  (transduce (eft/reductions focus-rf s)
             conj
             []
             ls))

;; TODO work out how to remove this loop and use hofs
(defn coll-lens-over
  [ls f s]
  (let [last-lens (peek ls)
        ls (pop ls)
        steps (coll-lens-focii ls s)]
    (loop [ss (pop steps)
           ls ls
           s (-over last-lens f (peek steps))]
      (if-let [l (peek ls)]
        (recur (pop ss) (pop ls) (put l s (peek ss)))
        s))))

;; * collection lenses

(extend clojure.lang.PersistentVector
  Focus
  {:-focus coll-lens-focus}
  Over
  {:-over coll-lens-over})

;; * constructors

(defn lens
  [getfn updatefn]
  (fn
    ([s] (getfn s))
    ([s f] (updatefn s f))))

(defn lens-from-setter
  [getfn setfn]
  (fn
    ([s] (getfn s))
    ([s f] (setfn s (f (getfn s))))))

(defn units
  [a->b b->a]
  (fn
    ([s] (a->b s))
    ([s f] (b->a (f (a->b s))))))


;; * lenses

(defn id
  ([s] s)
  ([s f] (f s)))

(defn nth
  [i]
  (fn
    ([s] (c/nth s i))
    ([s f] (update s i f))))

(def first (nth 0))
(def second (nth 1))

(defn select-keys
  "structure."
  [ks]
  (fn
    ([s] (c/select-keys s ks))
    ([s f]
     (merge (apply dissoc s ks)
            (-> (c/select-keys s ks)
                f
                (c/select-keys ks))))))


(defn- bound [mn n mx]
  (-> n (max mn) (min mx)))

(defn- slice-focus
  [from to s n]
  (subvec s (bound 0 from n) (bound 0 to n)))

(defn slice
  [from to]
  (fn
    ([s]
     (let [n (count s)]
       (slice-focus from to s n)))
    ([s f]
     (let [n (count s)]
       (-> s
           (subvec 0 (bound 0 from n))
           (into (f (slice-focus from to s n)))
           (into (subvec s (bound 0 to n) n)))))))


;; TODO
;; next, rest, pop, peek, last, butlast?
;; keys, vals
;; dissocing (with sentinel? nil?)
;; predicate lens
;; derived atoms (take from lentes)


(comment
  "test coll-lens-focii"

  (require '[criterium.core :as criterium])

  (criterium.core/bench
   (into [] (reductions focus-rf {:foo {:bar {:baz 123}}} [:foo :bar :baz]))) ; =>

  ;; Execution time mean : 486.902769 ns
  ;; Execution time std-deviation : 10.257123 ns
  
  (criterium.core/bench
   (eft/produce
    (comp (eft/iterate apply-step)
          eft/til-nil
          (map second))
    conj
    []
    (list (seq [:foo :bar :baz]) {:foo {:bar {:baz 123}}})))
  
  ;; Execution time mean : 1.654460 µs
  ;; Execution time std-deviation : 40.260947 ns

  (criterium.core/bench
   (transduce (eft/reductions focus-rf {:foo {:bar {:baz 123}}}) conj [] [:foo :bar :baz])
   )

  ;; Execution time mean : 154.007978 ns
  ;; Execution time std-deviation : 3.201630 ns

  (criterium.core/bench
   (transduce (eft/reductions eft/apply {:foo {:bar {:baz 123}}}) conj [] [:foo :bar :baz]))

  ;; Execution time mean : 134.500645 ns
  ;; Execution time std-deviation : 2.844009 ns


  (criterium.core/bench
   (assoc {:foo 1} :foo 2))

  ;; Execution time mean : 16.718223 ns
  ;; Execution time std-deviation : 0.697288 ns

  (criterium.core/bench
   (update {:foo 1} :foo (constantly 2)))

  ;; Execution time mean : 20.902394 ns
  ;; Execution time std-deviation : 0.650362 ns

  ,)


(comment
  "testing coll-lens-over"

  (over [:foo :bar :baz] inc {:foo {:bar {:baz 42}}})

  (put [:foo :bar :baz] 77 {:foo {:bar {:baz 42}}})



  ,)

(comment

  (over [:foo (units str parse-long)] clojure.string/reverse {:foo 21})


  ,)

(comment
  (over [:foo (slice 2 4)] #(map inc %) {:foo [1 1 1 1 1 ]})

  ,)

(comment

  (def m {:foo {:bar {:baz 42}}})
  (def ks [:foo :bar :baz])

  (require '[criterium.core])
  
  (criterium.core/bench
   (over ks inc m))
  ;; Execution time mean : 492.630408 ns

  (criterium.core/bench
   (update-in m ks inc)))
  ;; Execution time mean : 208.739133 ns

  (criterium.core/bench
   (put ks 77 m) )
  ;; Execution time mean : 943.873561 ns

  (criterium.core/bench
   (assoc-in m ks 77) )
  ;; Execution time mean : 239.439433 ns

  (criterium.core/bench
   (focus ks m))
  ;; Execution time mean : 68.952018 ns

  (criterium.core/bench
   (get-in m ks))
  ;; Execution time mean : 97.500455 ns


,

