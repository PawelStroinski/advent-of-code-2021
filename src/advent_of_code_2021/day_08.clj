(ns advent-of-code-2021.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (with-open [rdr (-> (io/resource "day_08") (io/reader))]
    (let [parse-signals #(map set (str/split % #" "))]
      (->> (line-seq rdr)
           (map #(str/split % #" \| "))
           (map (partial map parse-signals))
           (mapv (fn [[a b]] {:signal-patterns a :output-value b}))))))

(def unique-n-segments #{2 4 3 7})

(->> input
     (map :output-value)
     (map (partial map count))
     (mapcat (partial filter unique-n-segments))
     (count))

;;;;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;

(defn order-patterns
  [patterns]
  (let [fnd (fn [n-segments & preds]
              (->> patterns
                   (filter (apply every-pred #(= (count %) n-segments) preds))
                   (first)))
        one (fnd 2)
        four (fnd 4)
        seven (fnd 3)
        eight (fnd 7)
        cf one
        bd (set/difference four cf)
        zero (fnd 6 #(= (count (set/intersection % bd)) 1))
        b (first (set/intersection zero bd))
        d (first (set/difference bd #{b}))
        nine (fnd 6 #(= (set/intersection % cf) cf) #(contains? % d))
        e (first (set/difference eight nine))
        six (fnd 6 #(contains? % d) #(contains? % e))
        two (fnd 5 #(contains? % e))
        three (fnd 5 #(= (set/intersection % cf) cf))
        five (fnd 5 #(contains? % b))]
    [zero one two three four five six seven eight nine]))

(defn decode
  [{:keys [signal-patterns output-value]}]
  (let [ordered (order-patterns signal-patterns)
        digit #(.indexOf ordered %)
        addend (fn [v exponent] (* (digit v) (long (Math/pow 10 exponent))))
        exponents (-> output-value count range reverse)]
    (->> (map addend output-value exponents)
         (apply +))))

(->> input
     (map decode)
     (apply +))
