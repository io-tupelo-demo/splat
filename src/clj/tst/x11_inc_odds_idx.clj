(ns tst.x11-inc-odds-idx
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
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
