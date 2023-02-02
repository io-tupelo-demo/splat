(ns demo.core
  (:use tupelo.core)
  (:require
    [schema.core :as s])
  (:gen-class))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

(s/defn add2 :- s/Num
  "An example to demonstrate Plumatic Schema."
  [x :- s/Num
   y :- s/Num]
  (+ x y))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "-main - enter")
  (println "-main - leave"))
