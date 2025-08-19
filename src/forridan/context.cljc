(ns forridan.context
  (:require [forridan.flock :as flock]
            [soliton.core :as s]))

(defrecord FlockContext [flock])

(defn flock-context
  ([fns] (flock-context fns nil))
  ([fns context]
   (cond
     (seq? fns) (FlockContext. (flock/seq-flock fns) nil context)
     (vector? fns) (FlockContext. (flock/vec-flock fns) nil context)
     (map? fns) (FlockContext. (flock/tree-flock fns) nil context)
     :else (throw (ex-info (str "Can't make flock-context from" (type fns))
                           {:fns fns :context context})))))

(defn flock-step
  [c]
  (let [flock (flock/rotate (:flock c))]
    (when flock
      ((flock/leader flock) (assoc c :flock flock)))))

(defrecord SMContext [state transition-fn])

(defn sm-context
  [transition-fn context]
  (SMContext. nil transition-fn nil context))

(defn sm-step
  [c]
  (let [new-state ((:transition-fn c) c)]
    (when new-state
      (new-state (assoc c :state new-state)))))

(defrecord SimpleContext [step-fn])

(defn simple-context
  [step-fn context]
  (SimpleContext. step-fn nil context))

(defn simple-step
  [c]
  ((:step-fn c) c))

(defn decontext
  [c]
  #?(:clj
     (.-__extmap ^clojure.lang.IRecord c)
     :cljs
     (.-__extmap  ^cljs.core/Record c)))
