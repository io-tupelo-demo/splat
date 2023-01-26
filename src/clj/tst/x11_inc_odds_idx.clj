(ns tst.x11-inc-odds-idx
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

; Map keys can be strings, no problem.
(verify
  (is= 10
    (get-in {"a" {"b" 10}} ["a" "b"])))