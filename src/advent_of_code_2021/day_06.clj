(ns advent-of-code-2021.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day_06")
      (slurp)
      (str/split #"\D")
      (as-> strs (map #(Integer/parseInt %) strs))))

(defn part-1-day
  [age]
  (if (zero? age)
    [6 8]
    [(dec age)]))

(defn part-1
  [input days]
  (loop [lanternfish input
         days-left days]
    (if (zero? days-left)
      (count lanternfish)
      (recur (mapcat part-1-day lanternfish) (dec days-left)))))

(comment (part-1 input 80))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(defn part-2-day
  [lanternfish age-group]
  (let [how-many (get lanternfish age-group)]
    (cond-> lanternfish
      true (assoc age-group 0)
      (zero? age-group) (-> (update 7 + how-many)
                            (assoc 9 how-many))
      (pos? age-group) (assoc (dec age-group) how-many))))

(defn part-2
  [input days]
  (loop [lanternfish (merge (zipmap (range 0 10) (repeat 0))
                            (frequencies input))
         days-left days]
    (if (zero? days-left)
      (->> lanternfish vals (apply +))
      (recur (reduce part-2-day lanternfish (range 0 10)) (dec days-left)))))

(part-2 input 256)
