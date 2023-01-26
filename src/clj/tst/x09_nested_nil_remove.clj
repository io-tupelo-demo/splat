(ns tst.x09-nested-nil-remove
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.splat :as splat]
    ))

; Remove all `nil` values from a nested sequences
(verify
  (let [data       {:a [1 2 nil 3 nil [4 nil 5]]}
        expected   {:a [1 2 3 [4 5]]}
        data-splat (splat/splatter data)
        ]
    (is= data-splat
      {:entries
       #{{:key  {:data :a :type :prim}
          :type :map/entry
          :val
          {:entries
           #{{:idx 0 :type :list/entry :val {:data 1 :type :prim}}
             {:idx 1 :type :list/entry :val {:data 2 :type :prim}}
             {:idx 2 :type :list/entry :val {:data nil :type :prim}}
             {:idx 3 :type :list/entry :val {:data 3 :type :prim}}
             {:idx 4 :type :list/entry :val {:data nil :type :prim}}
             {:idx  5
              :type :list/entry
              :val  {:entries #{{:idx 0 :type :list/entry :val {:data 4 :type :prim}}
                                {:idx 1 :type :list/entry :val {:data nil :type :prim}}
                                {:idx 2 :type :list/entry :val {:data 5 :type :prim}}}
                     :type    :coll/list}}}
           :type :coll/list}}}
       :type :coll/map}
      )
    ; Solution is super-easy. Just return `nil` for any list-entry you wish to delete
    (let [intc   {:leave (fn [stack node]
                           (let [stack-pat [{:type :list/entry}]
                                 result    (if (and
                                                 (submatch? stack-pat stack)
                                                 (= :prim (grab :type node))
                                                 (nil? (grab :data node)))
                                             nil
                                             node)]
                             result))}
          result (splat/splatter-walk intc data)]
      (is= result expected)

      ; Only remove `nil` values that are a list-entry
      (is= {:a nil :b [1 2 #{nil 4 5}]}
        (splat/splatter-walk intc {:a nil :b [1 2 nil #{nil 4 5}]})))))

; We can implement an "auto-vacuum" using plain postwalk easily enough
(verify
  (let [data {:a {:b {:c {}}}}
        auto-vacuum (fn [arg]
                      (->> arg
                        (walk/postwalk (fn [item]
                                         (cond-it-> item
                                           (and (map-entry? item) (= {} (val item))) {})))))]
    (is= {} (auto-vacuum data))))
