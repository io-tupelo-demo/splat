(ns demo.core
  (:use tupelo.core)
  (:require
    [schema.core :as s])
  (:gen-class))

(defn safe-even? [arg] (and (int? arg) (even? arg)))
(defn safe-odd? [arg] (and (int? arg) (odd? arg)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "-main - enter")
  (println "-main - leave"))
