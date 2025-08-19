(ns forridan.flock
  (:require [zing.zip :as zip]))

(defprotocol Flock
  (-leader [this])
  (-rotate [this]))

(defn seq-flock
  [fns]
  (conj (apply list fns) :root))

(extend-protocol Flock
  #?(:clj clojure.lang.IPersistentList :cljs cljs.core/List)
  (-leader [fns] (first fns))
  (-rotate [fns] (next fns)))

(defn vec-flock
  [fns]
  [(into [:root] (seq fns)) 0])

(extend-protocol Flock
  #?(:clj clojure.lang.IPersistentVector :cljs cljs.core/PersistentVector)
  (-leader [[fns idx]] (fns idx))
  (-rotate [[fns idx]] (let [idx (inc idx)]
                        (when (< idx (count fns))
                          [fns idx]))))

(extend-protocol Flock
  zing.zip.TreeZipper
  (-leader [m] (zip/node m))
  (-rotate [m] (zip/next-leaf m)))

(defn tree-flock
  [fns-or-map]
  (if (map? fns-or-map)
    (zip/tree-zip fns-or-map)
    (zip/tree-zip {:label :root, :children (vec fns-or-map)})))

(defn leader
  [flock]
  (-leader flock))

(defn rotate
  [flock]
  (-rotate flock))
