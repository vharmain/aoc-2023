(ns day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_01_input.txt")))

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

;;; Part 1 ;;;

(->> input
     (str/split-lines)
     (map (fn [s] (keep digit-chars s)))
     (map (juxt first last))
     (map (fn [cs] (apply str cs)))
     (map read-string)
     (apply +))
;; => 53386

;;; Part 2 ;;;

(def word->digit
  {"one"   \1
   "two"   \2
   "three" \3
   "four"  \4
   "five"  \5
   "six"   \6
   "seven" \7
   "eight" \8
   "nine"  \9})


(def lookup-regex
  (re-pattern (format "(%s)" (str/join "|" (into (keys word->digit) (vals word->digit))))))

(defn lookup-first-digit
  [s]
  (first (re-find lookup-regex s)))

(def reverse-lookup-regex
  (->> (keys word->digit)
       (map str/reverse)
       (into (vals word->digit))
       (str/join "|")
       (format "(%s)")
       (re-pattern)))

(defn lookup-last-digit
  [s]
  (->> s
       str/reverse
       (re-find reverse-lookup-regex)
       first
       str/reverse))

(->> input
     (str/split-lines)
     (map (juxt lookup-first-digit lookup-last-digit))
     (map (fn [[first-digit last-digit]]
            [(get word->digit first-digit first-digit)
             (get word->digit last-digit last-digit)]))
     (map #(apply str %))
     (map read-string)
     (apply +))
;; => 53312
