(ns advent-of-code-2021.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (io/resource "day_10") (slurp) (str/split-lines)))

(def open->close
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def illegal-char->points
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn parse
  [line]
  (loop [p 0
         expected-close-stack '()]
    (let [c (get line p)
          new-close (get open->close c)]
      (cond
        (nil? c) {:expected-close-stack expected-close-stack}
        new-close (recur (inc p) (conj expected-close-stack new-close))
        (= (peek expected-close-stack) c) (recur (inc p) (pop expected-close-stack))
        :else {:first-illegal-char c}))))

(->> input
     (keep (comp :first-illegal-char parse))
     (map illegal-char->points)
     (apply +))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(def completion-char->points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score
  [chars]
  (reduce (fn [acc c] (+ (* acc 5) (completion-char->points c))) 0 chars))

(->> input
     (keep (comp :expected-close-stack parse))
     (map score)
     (sort)
     (#(nth % (/ (count %) 2))))
