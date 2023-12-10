(ns day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_03_input.txt")))

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn parse
  [s]
  (let [lines  (str/split-lines s)
        height (count lines)
        width  (count (first lines))]
    {:grid/width  width
     :grid/height height
     :grid/coords (for [[line i] (map vector lines (range))
                        [sym j]  (map vector line (range))]
                    [[i j] sym])}))

(defn coords-around
  [[x y]]
  (for [xd    (range -1 2)
        yd    (range -1 2)
        :when (not= [xd yd] [0 0])]
    [(+ x xd) (+ y yd)]))

(defn classify-coords
  [{:grid/keys [coords] :as grid}]
  (reduce (fn [grid [coords x]]
               (cond
                 (= \. x)                  (assoc-in grid [:grid/blanks coords] x)
                 (contains? digit-chars x) (assoc-in grid [:grid/nums coords] x)
                 :else                     (assoc-in grid [:grid/syms coords] x)))
          grid
          coords))

(defn find-potential-partnumbers
  [{:grid/keys [coords nums] :as grid}]
  (assoc grid :grid/potential-partnumbers
         (->> coords
              (partition-by ffirst)
              (mapcat (fn [row]
                        (->> row
                             (partition-by (fn [[coords _x]] (contains? nums coords)))
                             (filter (fn [coll] (some #(contains? nums (first %)) coll))))))
              (map (fn [num-group]
                     {:coords (into #{} (map first num-group))
                      :num    (->> num-group (map second) (apply str) read-string)})))))

(defn filter-nums-with-adjacent-syms
  [{:grid/keys [syms potential-partnumbers] :as _grid}]
  (filter (fn [{:keys [coords _num]}]
            (some (fn [coords]
                    (some #(contains? syms %) (coords-around coords)))
                  coords))
          potential-partnumbers))

(-> input
      parse
      classify-coords
      find-potential-partnumbers
      filter-nums-with-adjacent-syms
      (->> (map :num)
           (apply +)))
