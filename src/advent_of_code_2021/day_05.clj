(ns advent-of-code-2021.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line
  [s]
  (->> (str/split s #"\D")
       (remove #{""})
       (map #(Integer/parseInt %))))

(defn range' [a b] (if (<= a b) (range a (inc b)) (reverse (range b (inc a)))))

(defn line->points-1
  [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (map (fn [y] [x1 y]) (range' y1 y2))
    (= y1 y2) (map (fn [x] [x y1]) (range' x1 x2))))

(defn main
  [line->points]
  (with-open [rdr (-> (io/resource "day_05") (io/reader))]
    (->> (line-seq rdr)
         (mapcat (comp line->points parse-line))
         (frequencies)
         (vals)
         (filter #(>= % 2))
         (count))))

(main line->points-1)

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(defn line->points-2
  [[x1 y1 x2 y2]]
  (cond
    (= x1 x2) (map (fn [y] [x1 y]) (range' y1 y2))
    (= y1 y2) (map (fn [x] [x y1]) (range' x1 x2))
    :else (map vector (range' x1 x2) (range' y1 y2))))

(main line->points-2)
