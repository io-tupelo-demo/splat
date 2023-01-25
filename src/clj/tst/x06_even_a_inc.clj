(ns tst.x06-even-a-inc
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.splat :as splat]
    ))

; Increment all even values for :a keys in seq of maps
(verify
  (let [data            [{:a 1 :b 2} {:a 2} {:a 4} {:a 3}]
        result-expected [{:a 1 :b 2} {:a 3} {:a 5} {:a 3}]]
    ; Look at the data for a single frame when the primative 4 is found
    (let [intc-spy {:leave (fn [stack node]
                             (when (= 4 (:data node))
                               (splat/stack-spy stack node)))}]
      (when false
        (splat/splatter-walk-noop intc-spy data)
        (comment
          ; node =>
          {:branch :map/val :data 4 :type :prim}
          ; stack =>
          [{:key  {:data :a :type :prim}
            :type :map/entry
            :val  {:data 4 :type :prim}}
           {:branch :list/val :type :coll/map}
           {:idx 2 :type :list/entry :val {:type :coll/map}}
           {:type :coll/list}]
          )))

    ; We can match on both a stack pattern and a node pattern. Just copy the output of
    ; `splatter-walk-noop` and `stack-spy` and delete the parts you don't want or need
    ; to get `stack-pat` and the `if` condition
    (let [intc   {:leave (fn [stack node]
                           ; either of these stack-pat-* values will work
                           (let [stack-pat-long  [{:type :map/entry :key {:data :a}}
                                                  {:type :coll/map}
                                                  {:type :list/entry}
                                                  {:type :coll/list}
                                                  ]
                                 stack-pat-short [{:type :map/entry :key {:data :a}}
                                                  ]
                                 result          (if (and (safe-even? (:data node))
                                                       (submatch? stack-pat-short stack))
                                                   (update-in node [:data] inc)
                                                   node)]
                             result))}
          result (splat/splatter-walk intc data)]
      (is= result result-expected))

    ))

