(ns tst.x10-dissoc-in
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [tupelo.splat :as splat]
    [tupelo.core :as t]
    ))

; Easy problem, just use tupelo.core/dissoc-in
(verify
  (let [data       {:a {:b {:c 1}}}
        expected   {:a {:b {}}}]
    (is= expected (t/dissoc-in data [:a :b :c]) )))


