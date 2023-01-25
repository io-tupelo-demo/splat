(ns tst.x02-append-seq
  (:use tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    ))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

(verify
  (let [data       {:a [1 2 3]}
        data-splat (splat/splatter data)
        result (update-in data [:a]  #(glue % [4 5]))
        ]
    (is= result {:a [1 2 3 4 5]})))
