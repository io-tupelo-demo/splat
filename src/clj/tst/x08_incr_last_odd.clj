(ns tst.x08-incr-last-odd
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.splat :as splat]
    ))

; Increment all even values for :a keys in seq of maps
(verify
  (let [data       [2 1 3 6 9 4 8]
        expected   [2 1 3 6 10 4 8]
        data-splat (splat/splatter data)
        ]
    (is= data-splat
      {:entries #{{:idx 0 :type :list/entry :val {:data 2 :type :prim}}
                  {:idx 1 :type :list/entry :val {:data 1 :type :prim}}
                  {:idx 2 :type :list/entry :val {:data 3 :type :prim}}
                  {:idx 3 :type :list/entry :val {:data 6 :type :prim}}
                  {:idx 4 :type :list/entry :val {:data 9 :type :prim}}
                  {:idx 5 :type :list/entry :val {:data 4 :type :prim}}
                  {:idx 6 :type :list/entry :val {:data 8 :type :prim}}}
       :type    :coll/list})
    (let [odd-entries (keep-if (fn [entry]
                                 (safe-odd? (get-in entry [:val :data])))
                        (:entries data-splat))

          idx-max     (apply max (mapv :idx odd-entries))
          ]
      (is-set= odd-entries
        #{{:idx 1 :type :list/entry :val {:data 1 :type :prim}}
          {:idx 2 :type :list/entry :val {:data 3 :type :prim}}
          {:idx 4 :type :list/entry :val {:data 9 :type :prim}}})
      (is= idx-max 4)
      (let [intc-spy {:leave (fn [stack node]
                               (when (= 9 (:data node))
                                 (splat/stack-spy stack node)))}]
        (when false ; uncomment to print result
          (splat/splatter-walk-noop intc-spy data)
          (comment
            ; node =>
            {:branch :list/val :data 9 :type :prim}
            ; stack =>
            [{:idx 4 :type :list/entry :val {:data 9 :type :prim}}
             {:type :coll/list}])))
      (let [intc   {:leave (fn [stack node]
                             ; either of these stack-pat-* values will work
                             (let [stack-pat [{:type :list/entry :idx idx-max}
                                              {:type :coll/list}]
                                   result    (if (submatch? stack-pat stack)
                                               (update-in node [:data] inc)
                                               node)]
                               result))}
            result (splat/splatter-walk intc data)]
        (is= result expected)))
    ))

