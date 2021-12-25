(ns advent-of-code-2021.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-command
  [s]
  (let [[command x] (str/split s #" ")]
    {:command (keyword command)
     :x (Integer/parseInt x)}))

(def input (with-open [rdr (-> (io/resource "day_02") (io/reader))]
             (mapv parse-command (line-seq rdr))))

(defn move
  [acc {:keys [command x]}]
  (case command
    :forward (update acc :horizontal-position + x)
    :down (update acc :depth + x)
    :up (update acc :depth - x)))

(def starting-position {:horizontal-position 0 :depth 0})

(defn answer [m] (* (:horizontal-position m) (:depth m)))

(->> input
     (reduce move starting-position)
     (answer))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(def starting-position-2 {:horizontal-position 0 :depth 0 :aim 0})

(defn move-2
  [{:keys [aim] :as acc} {:keys [command x]}]
  (case command
    :forward (-> acc
                 (update :horizontal-position + x)
                 (update :depth + (* aim x)))
    :down (update acc :aim + x)
    :up (update acc :aim - x)))

(->> input
     (reduce move-2 starting-position-2)
     (answer))
