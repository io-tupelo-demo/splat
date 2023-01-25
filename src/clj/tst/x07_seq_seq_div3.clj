(ns tst.x07-seq-seq-div3
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.splat :as splat]
    ))

; Increment all even values for :a keys in seq of maps
(verify
  (let [data     [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]]
        expected [3 3 18 6 12]

        ;for something this simple, just use `forv`
        result   (forv [the-seq data
                        val     the-seq
                        :when (zero? (mod val 3))]
                   val)]
    (is= result expected)

    ))

