(ns tst.x01-map-list-map-even-inc
  (:use tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    ))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

(verify
  (is true)
  (let [data       {:a [{:aa 1 :bb 2}
                        {:cc 3}]
                    :b [{:dd 4}]
                    :c {:ee 0}
                    }
        intc-spy   {:leave (fn [stack node]
                             (when (= 2 (:data node))
                               (splat/stack-spy stack node)))}
        data-splat (splat/splatter data)]
    (is= data-splat
      {:type    :coll/map
       :entries #{{:type :map/entry
                   :key  {:data :a :type :prim}
                   :val  {:type    :coll/list
                          :entries #{{:type :list/entry
                                      :idx  0
                                      :val  {:type    :coll/map
                                             :entries #{{:type :map/entry
                                                         :key  {:data :aa :type :prim}
                                                         :val  {:data 1 :type :prim}}
                                                        {:type :map/entry
                                                         :key  {:data :bb :type :prim}
                                                         :val  {:data 2 :type :prim}}}}}
                                     {:type :list/entry
                                      :idx  1
                                      :val  {:type    :coll/map
                                             :entries #{{:type :map/entry
                                                         :key  {:data :cc :type :prim}
                                                         :val  {:data 3 :type :prim}}}}}}}}
                  {:type :map/entry
                   :key  {:data :b :type :prim}
                   :val  {:type    :coll/list
                          :entries #{{:type :list/entry
                                      :idx  0
                                      :val  {:type    :coll/map
                                             :entries #{{:type :map/entry
                                                         :key  {:data :dd :type :prim}
                                                         :val  {:data 4 :type :prim}}}}}}}}
                  {:type :map/entry
                   :key  {:data :c :type :prim}
                   :val  {:type    :coll/map
                          :entries #{{:type :map/entry
                                      :key  {:data :ee :type :prim}
                                      :val  {:data 0 :type :prim}}}}}}})

    ; (splat/splatter-walk-noop intc-spy data)
    (comment ; result
      ; node =>
      {:branch :map/val :data 2 :type :prim}
      ; stack =>
      [{:key  {:data :bb :type :prim}
        :type :map/entry
        :val  {:data 2 :type :prim}}
       {:branch :list/val :type :coll/map}
       {:idx 0 :type :list/entry :val {:type :coll/map}}
       {:branch :map/val :type :coll/list}
       {:key  {:data :a :type :prim}
        :type :map/entry
        :val  {:type :coll/list}}
       {:type :coll/map}])

    (let [intc {:leave (fn [stack node]
                         (let [data        (:data node)
                               stack-pat-6 [{:type :map/entry}
                                            {:type :coll/map}
                                            {:type :list/entry}
                                            {:type :coll/list}
                                            {:type :map/entry}
                                            {:type :coll/map}]
                               result      (cond-it-> node
                                             (and  (safe-even? data)
                                               (submatch? stack-pat-6 stack))
                                             (update-in node [:data] inc))]
                           result))}]
      (is= (splat/splatter-walk intc data)
        {:a [{:aa 1 :bb 3}
             {:cc 3}]
         :b [{:dd 5}]
         :c {:ee 0}}))
    (let [intc {:leave (fn [stack node]
                         (let [data        (:data node)
                               stack-pat-3 [{}
                                            {:type :coll/map}
                                            {}
                                            {:type :coll/list}
                                            ]
                               result      (cond-it-> node
                                             (and  (safe-even? data)
                                               (submatch? stack-pat-3 stack))
                                             (update-in node [:data] inc))]
                           result))}]
      (is= (splat/splatter-walk intc data)
        {:a [{:aa 1 :bb 3}
             {:cc 3}]
         :b [{:dd 5}]
         :c {:ee 0}}))
    (let [intc {:leave (fn [stack node]
                         (let [data        (:data node)
                               stack-pat-3 [{:type :map/entry}]
                               result      (cond-it-> node
                                             (and  (safe-even? data)
                                               (submatch? stack-pat-3 stack))
                                             (update-in node [:data] inc))]
                           result))}]
      (is= (splat/splatter-walk intc data)
        {:a [{:aa 1 :bb 3}
             {:cc 3}]
         :b [{:dd 5}]
         :c {:ee 1}}))

    (let [intc {:leave (fn [stack node]
                         (let [data        (:data node)
                               stack-pat-a [{}
                                            {:type :coll/map}
                                            {}
                                            {:type :coll/list}
                                            {:key {:data :a}}
                                            {:type :coll/map}]
                               result      (cond-it-> node
                                             (and  (safe-even? data)
                                               (submatch? stack-pat-a stack))
                                             (update-in node [:data] inc))]
                           result))}]
      (is= (splat/splatter-walk intc data)
        {:a [{:aa 1 :bb 3}
             {:cc 3}]
         :b [{:dd 4}]
         :c {:ee 0}}))
    (let [intc {:leave (fn [stack node]
                         (let [data        (:data node)
                               stack-pat-a [{:type :*}
                                            {:type :coll/map}
                                            :*
                                            {:type :coll/list}
                                            {:key {:data :a}}]
                               result      (cond-it-> node
                                             (and  (safe-even? data)
                                               (wild-submatch? stack-pat-a stack))
                                             (update-in node [:data] inc))]
                           result))}]
      (is= (splat/splatter-walk intc data)
        {:a [{:aa 1 :bb 3}
             {:cc 3}]
         :b [{:dd 4}]
         :c {:ee 0}}))

    ))

