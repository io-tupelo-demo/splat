(ns tst.x02-append-seq
  (:use tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    ))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

; For something so simple you don't need Spectre or Splatter
(verify
  (let [data   {:a [1 2 3]}
        result (update-in data [:a] #(glue % [4 5]))
        ]
    (is= result {:a [1 2 3 4 5]})))

; For something more complicated Splatter helps.  Suppose we want to append
; a value 666 to the end of every nested list?
; #todo #awt finish this
(verify
  (let [data       {:a [1 2 3]
                    :b {:c {:d [11 12 13]}}}
        data-splat (splat/splatter data)
        ]
    (is= data-splat
      {:entries #{{:key  {:data :a :type :prim}
                   :type :map/entry
                   :val  {:entries #{{:idx 0 :type :list/entry :val {:data 1 :type :prim}}
                                     {:idx 1 :type :list/entry :val {:data 2 :type :prim}}
                                     {:idx 2 :type :list/entry :val {:data 3 :type :prim}}}
                          :type    :coll/list}}
                  {:key  {:data :b :type :prim}
                   :type :map/entry
                   :val  {:entries #{{:key  {:data :c :type :prim}
                                      :type :map/entry
                                      :val  {:entries #{{:key  {:data :d :type :prim}
                                                         :type :map/entry
                                                         :val  {:entries #{{:idx  0
                                                                            :type :list/entry
                                                                            :val  {:data 11 :type :prim}}
                                                                           {:idx  1
                                                                            :type :list/entry
                                                                            :val  {:data 12 :type :prim}}
                                                                           {:idx  2
                                                                            :type :list/entry
                                                                            :val  {:data 13 :type :prim}}}
                                                                :type    :coll/list}}}
                                             :type    :coll/map}}}
                          :type    :coll/map}}}
       :type    :coll/map})


    ; Look at the data for a single frame when the 1
    (let [intc-spy {:leave (fn [stack node]
                             (when (= 1 (:data node))
                               (splat/stack-spy stack node)))}]
      (when false
        (splat/splatter-walk-noop intc-spy data)
        (comment
          ; node =>
          {:branch :list/val :data 1 :type :prim}
          ; stack =>
          [{:idx 0 :type :list/entry :val {:data 1 :type :prim}}
           {:branch :map/val :type :coll/list}
           {:key  {:data :a :type :prim}
            :type :map/entry
            :val  {:type :coll/list}}
           {:type :coll/map}]
          )))

    ; The easiest way to append to a sequence is to revert that sub-tree
    ; from a Splatter tree to a regular Clojure vector, then use tupelo.core/append
    ; to add the new value, then return it to Splatter format
    (let [intc {:leave (fn [stack node]
                         (if (= :coll/list (grab :type node))
                           (it-> node
                             (splat/unsplatter it)
                             (append it 666)
                             (splat/splatter it))
                           node))}]
      (is= (splat/splatter-walk intc data)
        {:a [1 2 3 666]
         :b {:c {:d [11 12 13 666]}}}))))
