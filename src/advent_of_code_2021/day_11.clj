(ns advent-of-code-2021.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> (io/resource "day_11")
                (slurp)
                (str/split-lines)
                (mapv (partial mapv #(Long/parseLong (str %))))))

(def all-points (for [y (range (-> input count))
                      x (range (-> input first count))]
                  [x y]))

(defn adjacent-points
  [x y]
  [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]
   [(dec x) (dec y)] [(dec x) (inc y)] [(inc x) (dec y)] [(inc x) (inc y)]])

(defn inc-all [energy-levels] (mapv (partial mapv inc) energy-levels))

(defn inc-adjacent
  [energy-levels x y]
  (reduce
    (fn [acc [x y]]
      (if (pos? (get-in acc [y x] 0))
        (update-in acc [y x] inc)
        acc))
    energy-levels
    (adjacent-points x y)))

(defn flash-maybe
  ([acc]
   (loop [acc acc]
     (let [new (reduce flash-maybe acc all-points)]
       (if (= new acc) acc (recur new)))))
  ([{:keys [energy-levels flashes] :as acc} [x y]]
   (if (> (get-in energy-levels [y x]) 9)
     {:energy-levels (-> energy-levels (inc-adjacent x y) (assoc-in [y x] 0))
      :flashes (inc flashes)}
     acc)))

(def init-acc {:energy-levels input :flashes 0})

(defn step [acc] (-> acc (update :energy-levels inc-all) flash-maybe))

(loop [acc init-acc
       steps-left 100]
  (if (pos? steps-left)
    (recur (step acc) (dec steps-left))
    acc))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(loop [acc init-acc
       step-no 1]
  (let [new-acc (step acc)
        step-flashes (- (:flashes new-acc) (:flashes acc))]
    (if (= step-flashes (count all-points))
      step-no
      (recur new-acc (inc step-no)))))
