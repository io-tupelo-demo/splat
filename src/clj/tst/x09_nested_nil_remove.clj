(ns tst.x09-nested-nil-remove
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    ))

; Remove all `nil` values from a nested sequence
(verify
  (let [data       {:a [1 2 nil 3 nil]}
        expected   {:a [1 2 3]}
        data-splat (splat/splatter data)
        ]
    (is= data-splat
      {:entries #{{:key  {:data :a :type :prim}
                   :type :map/entry
                   :val  {:entries #{{:idx 0 :type :list/entry :val {:data 1 :type :prim}}
                                     {:idx 1 :type :list/entry :val {:data 2 :type :prim}}
                                     {:idx 2 :type :list/entry :val {:data nil :type :prim}}
                                     {:idx 3 :type :list/entry :val {:data 3 :type :prim}}
                                     {:idx 4 :type :list/entry :val {:data nil :type :prim}}}
                          :type    :coll/list}}}
       :type    :coll/map}
      )
    ; Solution is super-easy. Just return `nil` for any node you wish to delete

    ; #todo: update so a nil `:val` will cascade to a nil list/entry
    (let [intc   {:leave (fn [stack node]
                           (let [result (if (and (= :list/entry (:type node))
                                              (nil? (get-in node [:val :data])))
                                          nil
                                          node)]
                             result))}
          result (splat/splatter-walk intc data)]
      (is= result expected))
    ))

