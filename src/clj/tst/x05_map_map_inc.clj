(ns tst.x05-map-map-inc
  (:use demo.core tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.splat :as splat]
    ))


; Increment all values in map of maps
(verify
  (let [data            {:a {:aa 1}
                         :b {:ba -1 :bb 2}}
        result-expected {:a {:aa 2}
                         :b {:ba 0 :bb 3}}

        ; for very simple data, can just use `postwalk`
        result-postwalk (walk/postwalk (fn [arg]
                                         (cond-it-> arg
                                           (number? it) (inc it)))
                          data)

        result-mapping (map-vals data
                         (fn [arg]
                           (map-vals arg safe-inc)))
        ]
    (is= result-expected result-postwalk)
    (is= result-expected result-mapping)

    ; For fun, lets look at the splatter data
    (let [data-splat (splat/splatter data)]
      (is= data-splat {:entries #{{:key  {:data :a :type :prim}
                                   :type :map/entry
                                   :val  {:entries #{{:key  {:data :aa :type :prim}
                                                      :type :map/entry
                                                      :val  {:data 1 :type :prim}}}
                                          :type    :coll/map}}
                                  {:key  {:data :b :type :prim}
                                   :type :map/entry
                                   :val  {:entries #{{:key  {:data :ba :type :prim}
                                                      :type :map/entry
                                                      :val  {:data -1 :type :prim}}
                                                     {:key  {:data :bb :type :prim}
                                                      :type :map/entry
                                                      :val  {:data 2 :type :prim}}}
                                          :type    :coll/map}}}
                       :type    :coll/map})

      ; Look at the data for a single frame, when the primative 2 is found
      (let [intc-spy {:leave (fn [stack node]
                               (when (= 2 (:data node))
                                 (splat/stack-spy stack node)))}]
        (when false
          (splat/splatter-walk-noop intc-spy data)
          (comment
            ; node =>
            {:branch :map/val :data 2 :type :prim}
            ; stack =>
            [{:key  {:data :bb :type :prim}
              :type :map/entry
              :val  {:data 2 :type :prim}}
             {:branch :map/val :type :coll/map}
             {:key  {:data :b :type :prim}
              :type :map/entry
              :val  {:type :coll/map}}
             {:type :coll/map}])))

      ; We can match on both a stack pattern and a node pattern. Just copy the output of
      ; `splatter-walk-noop` and `stack-spy` and delete the parts you don't want or need
      ; to get `stack-pat` and the `if` condition
      (let [intc   {:leave (fn [stack node]
                             (let [stack-pat [{:type :map/entry}
                                              {:type :coll/map :branch :map/val}
                                              {:type :map/entry}
                                              {:type :coll/map}]
                                   result    (if (and (submatch? {:branch :map/val :type :prim} node)
                                                   (submatch? stack-pat stack))
                                               (update-in node [:data] inc)
                                               node)]
                               result))}
            result (splat/splatter-walk intc data)]
        (is= result result-expected))

      )))

