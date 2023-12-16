(ns day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day_05_input.txt")))

(defn parse
  [s]
  (let [[line1 & lines] (str/split-lines s)]
    (into
     {:seeds (-> line1
                 (str/split #":")
                 second
                 (->> (format "#{%s}")
                      (read-string)))}
     (-> lines
         (->> (remove empty?)
              (partition-by #(str/ends-with? % ":"))
              (apply hash-map))
         (update-keys #(-> % first (str/split #" ") first keyword))
         (update-vals (fn [coll]
                        (->> coll
                             (map #(read-string (format "[%s]" %)))
                             (map (fn [[n1 n2 n3]]
                                       {:destination-range-start n1
                                        :source-range-start      n2
                                        :range-length            n3})))))))))


(defn traverse
  [m res path n]
  (let [k  (first path)
        m1 (some (fn [{:keys [source-range-start range-length destination-range-start] :as m1}]
                   (when (>= (dec (+ source-range-start range-length)) n source-range-start)
                     (let [output (+ destination-range-start (- n source-range-start))]
                       (assoc m1 :input n :output output))))
                 (get m k))]
    (if (and m1 (seq path))
      (recur m (assoc res k m1) (rest path) (:output m1))
      res)))

(def data (parse input))

(def path [:seed-to-soil
           :soil-to-fertilizer
           :fertilizer-to-water
           :water-to-light
           :light-to-temperature
           :temperature-to-humidity
           :humidity-to-location])

(-> data
    :seeds
    (->> (map #(traverse data {} path %))
         (map (comp :output :humidity-to-location))
         (remove nil?)
         (apply min)))
;; => 88151870
