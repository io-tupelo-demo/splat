(ns ^:test-refresh/focus
  tst.x11-inc-odds-idx
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.core :as t]
    [tupelo.splat :as splat]
    ))

; Increment all odd values with an index in the slice [1..4)
(verify
  (let [data     [0 1 2 3 4 5 6 7]
        expected [0 2 2 4 4 5 6 7]
        result   (forv [[idx val] (indexed data)]
                   (if (and (odd? val)
                         (<= 1 idx 3))
                     (inc val)
                     val))]

    (is= expected result)))

; Replace the subsequence from indices 2 to 4 with [:a :b :c :d :e]
(verify
  (let [data     [0 1 2 3 4 5 6 7 8 9]
        expected [0 1 :a :b :c :d :e 4 5 6 7 8 9]
        result   (glue
                   (take 2 data)
                   [:a :b :c :d :e]
                   (drop 4 data))]
    (is= expected result)))

; Find all numbers in a deeply nested data structure
(verify
  (let [data     {2 [1 2 [6 7]] :a 4 :c {:a 1 :d [2 nil]}}
        expected [2 1 2 6 7 4 1 2]
        result   (t/with-cum-vector ; easy way to accumulate results
                   (->> data
                     (walk/postwalk (fn [arg]
                                      (when (number? arg)
                                        (t/cum-vector-append! arg))))))]
    (is= expected result)))

; Map keys can be strings no problem.
(verify
  (is= 10
    (get-in {"a" {"b" 10}} ["a" "b"])))

; Increment all even numbers but only at 2nd level deep
(verify
  (let [data     [{:a  1
                   :aa 2
                   :b  {:c 3
                        :d 4}
                   :e  {:f 6
                        :g 7
                        :h {:i 9
                            :j 10}}}]
        expected [{:a  1
                   :aa 2
                   :b  {:c 3
                        :d 5}
                   :e  {:f 7
                        :g 7
                        :h {:i 9
                            :j 10}}}]]

    ; Use the `splat/stack-spy` function to display the frame (node+stack) when
    ; we encounter a valid target like `4`
    (let [intc-spy {:leave (fn [stack node]
                             (when (= 4 (:data node))
                               (splat/stack-spy stack node)))}]
      (when false ; enable to print result
        (splat/splatter-walk-noop intc-spy data)
        (comment
          ; node =>
          {:branch :map/val :data 4 :type :prim}
          ; stack =>
          [{:key  {:data :d :type :prim}
            :type :map/entry
            :val  {:data 4 :type :prim}}
           {:branch :map/val :type :coll/map}
           {:key  {:data :b :type :prim}
            :type :map/entry
            :val  {:type :coll/map}}
           {:branch :list/val :type :coll/map}
           {:idx 0 :type :list/entry :val {:type :coll/map}}
           {:type :coll/list}])))

    ; We don't really need any data from the stack just the size to indicate
    ; the desired depth => 6
    (let [intc   {:leave (fn [stack node]
                           (if (and
                                 (= 6 (count stack))
                                 (safe-even? (:data node)))
                             (update-in node [:data] inc)
                             node))}
          result (splat/splatter-walk intc data)]
      (is= result expected))
    ))

; Add value of :b key to :a key if :a key value is even
(verify
  (let [data     [{:a 1 :b 5}
                  {:a 2 :b 7}
                  {:a 3 :b 11}
                  {:a 4 :b 13}]
        expected [{:a 1 :b 5}
                  {:a 9 :b 7}
                  {:a 3 :b 11}
                  {:a 17 :b 13}]]
    ; Can just use `forv` for this
    (let [result (forv [m data]
                   (with-map-vals m [a b]
                     (cond-it-> m
                       (even? a) (update-in m [:a] #(+ % b)))))]
      (is= expected result))))

; Apply namespace labels depending on the type of record
(verify
  (let [data     {:cust {:name "Joe"}
                  :city {:name "Springfield"}}
        expected {:cust {:cust/name "Joe"}
                  :city {:city/name "Springfield"}}]

    ; Use the `splat/stack-spy` function to display the frame (node+stack) when
    ; we encounter a valid target like `4`
    (let [intc-spy {:leave (fn [stack node]
                             (when (= :name (:data node))
                               (splat/stack-spy stack node)))}]
      (when false ; enable to print result
        (splat/splatter-walk-noop intc-spy data)
        (comment
          ; node =>
          {:branch :map/key :data :name :type :prim}
          ; stack =>
          [{:key  {:data :name :type :prim}
            :type :map/entry
            :val  {:data "Sprintfield" :type :prim}}
           {:branch :map/val :type :coll/map}
           {:key  {:data :city :type :prim}
            :type :map/entry
            :val  {:type :coll/map}}
           {:type :coll/map}]
          )))

    ; Add appropriate namespace prefix to `:name` keys
    (let [stack-tmpl-city [{:key  {:data :name :type :prim}
                            :type :map/entry}
                           {:type :coll/map}
                           {:key {:data :city :type :prim}}]

          stack-tmpl-cust [{:key  {:data :name :type :prim}
                            :type :map/entry}
                           {:type :coll/map}
                           {:key {:data :cust :type :prim}}]

          intc            {:leave (fn [stack node]
                                    ; Like `clojure.core/cont->`, `tupelo.core/cont-it->` will return the `node`
                                    ; value unchanged if none of the conditionals match.
                                    (cond-it-> node

                                      (and ; `it` is the threaded value of `node` in these expressions
                                        (submatch? {:branch :map/key :data :name} it)
                                        (submatch? stack-tmpl-cust stack))
                                      (assoc-in it [:data] :cust/name)

                                      (and ; `it` is the threaded value of `node` in these expressions
                                        (submatch? {:branch :map/key :data :name} it)
                                        (submatch? stack-tmpl-city stack))
                                      (assoc-in it [:data] :city/name)))}
          result          (splat/splatter-walk intc data)]
      (is= result expected))))

