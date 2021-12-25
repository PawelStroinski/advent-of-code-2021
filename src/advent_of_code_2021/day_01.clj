(ns advent-of-code-2021.day-01
  (:require [clojure.java.io :as io]))

(def input (with-open [rdr (-> (io/resource "day_01") (io/reader))]
             (mapv #(Integer/parseInt %) (line-seq rdr))))

(defn part-1
  [input]
  (->> input
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(part-1 input)

(defn part-2
  [input]
  (->> input
       (partition 3 1)
       (map (partial apply +))
       (part-1)))

(part-2 input)
