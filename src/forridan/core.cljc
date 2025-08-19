(ns forridan.core
  (:require [clojure.core.async :as a]
            [ergo.async :as ergo.async]
            [ergo.async-mixed :as ergo.mixed]
            [ergo.core :as ergo]
            [forridan.context :as context]))

(defn run
  ([context]
   (run context/flock-step context))
  ([stepfn context]
   (ergo/produce (comp (ergo.mixed/iterate stepfn)
                       ergo.core/til-nil)
                 ergo/last
                 nil
                 context)))

(defn steps
  ([context]
   (steps context/flock-step context))
  ([stepfn context]
   (let [result (a/chan 32)]
     (ergo/produce (comp (ergo.mixed/iterate stepfn)
                         ergo.core/til-nil)
                   ergo.async/put-rf!
                   result
                   context)
     result)))

(defn run-sync
  ([context]
   (run-sync context/flock-step context))
  ([stepfn context]
   (ergo/produce (comp (ergo/iterate stepfn)
                       ergo.core/til-nil)
                 ergo/last
                 nil
                 context)))

(defn steps-sync
  ([context]
   (steps-sync context/flock-step context))
  ([stepfn context]
   (ergo/produce (comp (ergo/iterate stepfn)
                       ergo.core/til-nil)
                 conj
                 []
                 context)))
