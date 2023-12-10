(ns day-04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_04_input.txt")))

(defn parse-card
  [s]
  (let [[s1 s2]   (str/split s #"\|")
        [ss1 ss2] (str/split s1 #":")]
    {:id           ss1
     :nums         (read-string (format "#{%s}" ss2))
     :winning-nums (read-string (format "#{%s}" s2))}))

(defn parse
  [s]
  (->> s
       (str/split-lines)
       (map parse-card)))

(defn calc-card-points
  [{:keys [nums winning-nums] :as card}]
  (assoc card :points (let [n (count (set/intersection nums winning-nums))]
                        (condp = n
                          0 n
                          1 n
                          (Math/pow 2 (dec n))))))

(-> input
    parse
    (->> (map calc-card-points)
         (map :points)
         (apply +)))
;; => 25651.0
