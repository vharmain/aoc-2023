(ns day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_01_input.txt")))

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(->> input
     (str/split-lines)
     (map (fn [s] (keep digit-chars s)))
     (map (juxt first last))
     (map (fn [cs] (apply str cs)))
     (map read-string)
     (apply +))
;; => 53386
