(ns advent-of-code-2021.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day_07")
      (slurp)
      (str/split #"\D")
      (as-> strs (map #(Long/parseLong %) strs))))

(defn part-1-fuel
  [^long target]
  (->> input
       (map (fn [^long x] (Math/abs (- x target))))
       (apply +)))

(defn main
  [fuel]
  (->> (range (apply min input) (inc (apply max input)))
       (map fuel)
       (apply min)))

(main part-1-fuel)

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(def distance->fuel
  (memoize
    (fn [distance] (apply + (range 1 (inc distance))))))

(defn part-2-fuel
  [^long target]
  (->> input
       (map (fn [^long x] (Math/abs (- x target))))
       (map distance->fuel)
       (apply +)))

(comment (main part-2-fuel))
