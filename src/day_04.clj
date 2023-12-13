(ns day-04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_04_input.txt")))

(defn parse-card
  [s]
  (let [[s1 s2]   (str/split s #"\|")
        [ss1 ss2] (str/split s1 #":")]
    {:id           (read-string (re-find #"[0-9]+" ss1))
     :nums         (read-string (format "#{%s}" ss2))
     :winning-nums (read-string (format "#{%s}" s2))}))

(defn parse
  [s]
  (->> s
       (str/split-lines)
       (map parse-card)))

(defn calc-card-points
  [{:keys [nums winning-nums] :as card}]
  (let [n (count (set/intersection nums winning-nums))]
    (assoc card
           :matching-count n
           :points (condp = n
                     0 n
                     1 n
                     (Math/pow 2 (dec n))))))

;;; Part 1 ;;;

(-> input
    parse
    (->> (map calc-card-points)
         (map :points)
         (apply +)))
;; => 25651.0

;;; Part 2 ;;;

(defn process
  [cards]
  (let [id->match-count (into {} (map (juxt :id :matching-count)) cards)
        init-count      (count cards)]
    (loop [cards-count    init-count
           ids-to-process (keys id->match-count)]
      (if-let [card-id (first ids-to-process)]
        (let [start  (inc card-id)
              end    (+ start (get id->match-count card-id))
              copies (keys (select-keys id->match-count (range start end)))]
          (recur (+ cards-count (count copies)) (into (rest ids-to-process) copies)))
        cards-count))))

(-> input
    parse
    (->> (map calc-card-points)
         process))
;; => 19499881
