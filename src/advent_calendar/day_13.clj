(ns advent-calendar.day-13
  (:require [clojure.string :as str]))

(defn calculate-happiness
  "Calculates the total happiness of an arrangement."
  [arrangement compatibility-map]
  (loop [i 1, sum 0]
    (let [prev (nth arrangement (dec i)), curr (nth arrangement i)]
      (if (= i (dec (count arrangement)))
        (+ sum (curr (prev compatibility-map)) (prev (curr compatibility-map)) (curr ((nth arrangement 0) compatibility-map)) ((nth arrangement 0) (curr compatibility-map)))
          (recur (inc i) (+ sum (curr (prev compatibility-map)) (prev (curr compatibility-map))))))))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn action-value
  "Returns the value of an action."
  [action]
  (cond
    (= action "lose") -1
    (= action "gain") 1))

(defn generate-compatibility-map
  "Generate map with compatibility between persons."
  [input]
  (let [compatibilities (str/split input #"\n")]
    (loop [i 0, distance-map {}]
      (if (>= i (count compatibilities))
        distance-map
        (let [[first would action amount happiness units by sitting next to second] (str/split (get compatibilities i) #" |\.")]
          (recur (inc i) (assoc-in distance-map [(keyword first) (keyword second)] (* (action-value action)(Integer/parseInt amount)))))))))

(defn add-yourself
  "Adds yourself to the compatibility map with all compatibilities set to 0."
  [compatibility-map]
  (let [persons (keys compatibility-map)]
    (loop [i 0, compatibility-map (assoc compatibility-map :you {})]
      (if (>= i (count persons))
        compatibility-map
        (recur (inc i) (assoc-in (assoc-in compatibility-map [:you (nth persons i)] 0) [(nth persons i) :you] 0))))))

(defn happiest-arrangement
  "Get happiest arrangement from input."
  [input]
  (let [compatibility-map (generate-compatibility-map input)]
    (reduce max (map #(calculate-happiness % compatibility-map) (permutations (keys compatibility-map))))))

(defn happiest-arrangement-with-yourself
  "Get happiest arrangement from input with yourself included."
  [input]
  (let [compatibility-map (add-yourself (generate-compatibility-map input))]
    (reduce max (map #(calculate-happiness % compatibility-map) (permutations (keys compatibility-map))))))

(def input "Alice would lose 57 happiness units by sitting next to Bob.\nAlice would lose 62 happiness units by sitting next to Carol.\nAlice would lose 75 happiness units by sitting next to David.\nAlice would gain 71 happiness units by sitting next to Eric.\nAlice would lose 22 happiness units by sitting next to Frank.\nAlice would lose 23 happiness units by sitting next to George.\nAlice would lose 76 happiness units by sitting next to Mallory.\nBob would lose 14 happiness units by sitting next to Alice.\nBob would gain 48 happiness units by sitting next to Carol.\nBob would gain 89 happiness units by sitting next to David.\nBob would gain 86 happiness units by sitting next to Eric.\nBob would lose 2 happiness units by sitting next to Frank.\nBob would gain 27 happiness units by sitting next to George.\nBob would gain 19 happiness units by sitting next to Mallory.\nCarol would gain 37 happiness units by sitting next to Alice.\nCarol would gain 45 happiness units by sitting next to Bob.\nCarol would gain 24 happiness units by sitting next to David.\nCarol would gain 5 happiness units by sitting next to Eric.\nCarol would lose 68 happiness units by sitting next to Frank.\nCarol would lose 25 happiness units by sitting next to George.\nCarol would gain 30 happiness units by sitting next to Mallory.\nDavid would lose 51 happiness units by sitting next to Alice.\nDavid would gain 34 happiness units by sitting next to Bob.\nDavid would gain 99 happiness units by sitting next to Carol.\nDavid would gain 91 happiness units by sitting next to Eric.\nDavid would lose 38 happiness units by sitting next to Frank.\nDavid would gain 60 happiness units by sitting next to George.\nDavid would lose 63 happiness units by sitting next to Mallory.\nEric would gain 23 happiness units by sitting next to Alice.\nEric would lose 69 happiness units by sitting next to Bob.\nEric would lose 33 happiness units by sitting next to Carol.\nEric would lose 47 happiness units by sitting next to David.\nEric would gain 75 happiness units by sitting next to Frank.\nEric would gain 82 happiness units by sitting next to George.\nEric would gain 13 happiness units by sitting next to Mallory.\nFrank would gain 77 happiness units by sitting next to Alice.\nFrank would gain 27 happiness units by sitting next to Bob.\nFrank would lose 87 happiness units by sitting next to Carol.\nFrank would gain 74 happiness units by sitting next to David.\nFrank would lose 41 happiness units by sitting next to Eric.\nFrank would lose 99 happiness units by sitting next to George.\nFrank would gain 26 happiness units by sitting next to Mallory.\nGeorge would lose 63 happiness units by sitting next to Alice.\nGeorge would lose 51 happiness units by sitting next to Bob.\nGeorge would lose 60 happiness units by sitting next to Carol.\nGeorge would gain 30 happiness units by sitting next to David.\nGeorge would lose 100 happiness units by sitting next to Eric.\nGeorge would lose 63 happiness units by sitting next to Frank.\nGeorge would gain 57 happiness units by sitting next to Mallory.\nMallory would lose 71 happiness units by sitting next to Alice.\nMallory would lose 28 happiness units by sitting next to Bob.\nMallory would lose 10 happiness units by sitting next to Carol.\nMallory would gain 44 happiness units by sitting next to David.\nMallory would gain 22 happiness units by sitting next to Eric.\nMallory would gain 79 happiness units by sitting next to Frank.\nMallory would lose 16 happiness units by sitting next to George.")
(println (happiest-arrangement input))
(println (happiest-arrangement-with-yourself input))

