(ns advent-of-code-2021.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [rdr (-> (io/resource "day_04") (io/reader))]
    (let [lines (line-seq rdr)
          random-order (first lines)
          boards (drop 2 lines)
          parse-int #(Integer/parseInt %)
          parse-board (fn [board]
                        (mapv
                          (fn [line]
                            (-> line
                                str/trim
                                (str/split #" +")
                                (as-> strs (mapv parse-int strs))))
                          board))]
      {:random-order (->> (str/split random-order #",")
                          (mapv parse-int))
       :boards (->> boards
                    (partition 5 6)
                    (mapv parse-board))})))

(defn mark-board
  [board draw]
  (mapv
    (partial mapv #(if (= draw %) {:marked %} %))
    board))

(defn board-wins?
  [board]
  (let [marked-row? #(some (partial not-any? number?) %)
        rotate #(apply map vector %)]
    (or (marked-row? board)
        (marked-row? (rotate board)))))

(defn score
  [board draw]
  (let [unmarked-sum (->> board
                          (map (partial filter number?))
                          (map (partial apply +))
                          (apply +))]
    (* unmarked-sum draw)))

(loop [[draw & next-random-order] (:random-order input)
       boards (:boards input)]
  (let [marked-boards (map #(mark-board % draw) boards)
        winning-board (first (filter board-wins? marked-boards))]
    (cond
      winning-board (score winning-board draw)
      next-random-order (recur next-random-order marked-boards)
      :else marked-boards)))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(loop [[draw & next-random-order] (:random-order input)
       boards (:boards input)
       last-score nil]
  (let [marked-boards (map #(mark-board % draw) boards)
        winning-boards (filter board-wins? marked-boards)
        unwinning-boards (seq (remove (set winning-boards) marked-boards))
        current-score (if-let [winning (first winning-boards)]
                        (score winning draw)
                        last-score)]
    (if (and next-random-order unwinning-boards)
      (recur next-random-order unwinning-boards current-score)
      current-score)))
