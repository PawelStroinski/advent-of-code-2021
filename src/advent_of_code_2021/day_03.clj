(ns advent-of-code-2021.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (io/resource "day_03") slurp str/split-lines))

(defn count-letter-occurencies-by-column
  [lines max-column]
  (reduce
    (fn [acc line]
      (reduce
        (fn [acc' [column letter]]
          (update-in acc' [column letter] (fnil inc 0)))
        acc
        (map vector (range max-column) line)))
    (vec (repeat max-column {}))
    lines))

(defn most-common [m] (->> m (sort-by val) (last) (key)))
(defn least-common [m] (->> m (sort-by val) (first) (key)))

(defn binary->decimal
  [binary]
  (->> binary
       (reverse)
       (map-indexed (fn [idx x] (if (= x \1) (int (Math/pow 2 idx)) 0)))
       (apply +)))

(let [line-length (-> input first count)
      counts (count-letter-occurencies-by-column input line-length)
      gamma (map most-common counts)
      epsilon (map least-common counts)]
  (* (binary->decimal gamma) (binary->decimal epsilon)))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-value
  [input if-more-zeros else]
  (loop [lines input
         index 0]
    (let [counts (peek (count-letter-occurencies-by-column lines (inc index)))
          letter (if (> (get counts \0 0) (get counts \1 0)) if-more-zeros else)
          has-letter-at-index? #(= letter (nth % index))
          left (filter has-letter-at-index? lines)]
      (if (> (count left) 1)
        (recur left (inc index))
        (first left)))))

(let [oxygen-generator-rating (find-value input \0 \1)
      co2-scrubber-rating (find-value input \1 \0)]
  (* (binary->decimal oxygen-generator-rating)
     (binary->decimal co2-scrubber-rating)))
