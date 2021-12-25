(ns advent-of-code-2021.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (io/resource "day_09") (slurp) (str/split-lines)))

(defn adjacent [x y] [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]])

(defn height [x y] (-> (get-in input [y x] \9) str Integer/parseInt))

(defn lowest-point?
  [x y point-height]
  (let [adjacent-heights (->> (adjacent x y)
                              (map (partial apply height)))]
    (every? (partial < point-height) adjacent-heights)))

(->> (for [y (range (-> input count))
           x (range (-> input first count))]
       [x y (height x y)])
     (keep (fn [[x y point-height]] (when (lowest-point? x y point-height)
                                      (inc point-height))))
     (apply +))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(defn lowest-point->basin-size
  [x y]
  (loop [queue [[x y]]
         visited #{}]
    (if-let [[x y] (first queue)]
      (recur (->> (adjacent x y)
                  (remove visited)
                  (remove (fn [[x' y']] (= (height x' y') 9)))
                  (concat (rest queue)))
             (conj visited [x y]))
      (count visited))))

(->> (for [y (range (-> input count))
           x (range (-> input first count))]
       [x y])
     (filter (fn [[x y]] (lowest-point? x y (height x y))))
     (map (partial apply lowest-point->basin-size))
     (sort)
     (take-last 3)
     (apply *))
