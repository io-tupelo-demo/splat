(ns tst.x02-append-seq
  (:use tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    ))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

; For something so simple, you don't need Spectre or Splatter
(verify
  (let [data       {:a [1 2 3]}
        result (update-in data [:a]  #(glue % [4 5]))
        ]
    (is= result {:a [1 2 3 4 5]})))

; For something more complicated, Splatter helps.  Suppose we want to append
; a value 666 to the end of every nested list?
; #todo #awt finish this
#_(verify
  (let [data       {:a [1 2 3]
                    :b {:c {:d [11 12 13]}}}
        data-splat (splat/splatter data)
        ]
    (spyx-pretty data-splat )
    ))
