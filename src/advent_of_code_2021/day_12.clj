(ns advent-of-code-2021.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(def input
  (with-open [rdr (-> (io/resource "day_12") (io/reader))]
    (->> (line-seq rdr)
         (map #(str/split % #"-"))
         (map #(map keyword %))
         (reduce (fn [acc [a b]] (merge-with concat acc {a [b] b [a]})) {}))))

(defn extend-queue
  [queue path can-visit-cave?]
  (into
    queue
    (keep #(when (can-visit-cave? % path) (conj path %)))
    (-> path peek input)))

(defn number-of-paths
  [can-visit-cave?]
  (loop [[path & queue] [[:start]]
         num 0]
    (cond
      (= (peek path) :end) (recur queue (inc num))
      path (recur (extend-queue queue path can-visit-cave?) num)
      :else num)))

(def big-cave?
  (memoize
    (fn [k] (Character/isUpperCase ^Character (-> k name (nth 0))))))

(defn part-1-can-visit-cave?
  [cave path]
  (or (not (contains? (set path) cave))
      (big-cave? cave)))

(number-of-paths part-1-can-visit-cave?)

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(def all-small-caves-distinct?
  (memoize
    (fn [path]
      (let [small-caves (remove big-cave? path)]
        (= (count small-caves) (count (set small-caves)))))))

(defn part-2-can-visit-cave?
  [cave path]
  (or (part-1-can-visit-cave? cave path)
      (and (not= cave :start)
           (all-small-caves-distinct? path))))

(comment (number-of-paths part-2-can-visit-cave?))
