(ns day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_02_input.txt")))

(defn parse-round
  [s]
  (-> s
      (str/split #",")
      (->> (map (fn [s] (-> s (str/trim) (str/split #" ") reverse vec)))
           (into {}))
      (update-keys keyword)
      (update-vals read-string)))

(defn parse-line
  [s]
  (let [[s1 s2] (str/split s #":")]
    {:game/id     (-> s1 (subs (count "Game ")) read-string)
     :game/rounds (-> s2 (str/split #";") (->> (map parse-round)))}))

(defn parse
  [s]
  (->> s
       (str/split-lines)
       (map parse-line)))

;;; Part 1 ;;;

(def constraints {:red 12 :green 13 :blue 14})

(defn possible-round?
  [constraints round]
  (every? (fn [k] (<= (k round) (k constraints))) (keys round)))

(->> input
     parse
     (filter (fn [{:game/keys [rounds]}]
               (every? #(possible-round? constraints %) rounds)))
     (map :game/id)
     (apply +))
;; => 2237

;;; Part 2 ;;;

(->> input
     parse
     (map (fn [{:game/keys [rounds] :as game}]
            (assoc game :game/maxes (apply merge-with max rounds))))
     (map :game/maxes)
     (map vals)
     (map #(apply * %))
     (apply +))
;; => 66681
