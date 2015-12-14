(ns advent-calendar.day-14
  (:require [clojure.string :as str]))

(defn get-reindeer-specs
  "Returns the specs of each raindeer."
  [input]
  (let [reindeers (str/split input #"\n")]
    (loop [i 0, specs {}]
      (if (>= i (count reindeers))
        specs
        (let [params (str/split (get reindeers i) #" ") name (keyword (get params 0)) speed (Integer/parseInt (get params 3)) duration (Integer/parseInt (get params 6)) cooldown (Integer/parseInt (get params 13))]
          (recur (inc i) (assoc-in (assoc-in (assoc-in specs [name :speed] speed)[name :duration] duration) [name :cooldown] cooldown)))))))

(defn advance-reindeers
  "Simulates a given second in a reindeer race."
  [specs second distances]
  (loop [i 0, distances distances]
    (if (>= i (count (keys specs)))
      distances
      (let [reindeer (nth (keys specs) i)]
        (recur (inc i) (assoc distances reindeer (+ (if (nil? (reindeer distances)) 0 (reindeer distances)) (if (< (mod second (+ (get-in specs [reindeer :duration]) (get-in specs [reindeer :cooldown]))) (get-in specs [reindeer :duration])) (get-in specs [reindeer :speed]) 0 ))))))))

(defn update-points
  "Updates the points for the reindeer race."
  [distances points]
  (let [first-place (keys (filter (fn [[k v]] (= v (reduce max (vals distances)))) distances))]
    (loop [j 0, updated-points points]
      (if (>= j (count first-place))
        updated-points
        (recur (inc j) (assoc updated-points (nth first-place j) (inc (if (nil? ((nth first-place j) points)) 0 ((nth first-place j) points)))))))))

(defn race
  "Races the reindeers and return the distances of the reindeers"
  [time specs]
  (loop [i 0, distances {}]
    (if (>= i time)
      distances
      (recur (inc i) (advance-reindeers specs i distances)))))

(defn points-race
  "Races the reindeers and return the points of the reindeers"
  [time specs]
  (loop [i 0, distances {}, points {}]
    (if (>= i time)
      points
      (let [updated-distances (advance-reindeers specs i distances)]
        (recur (inc i) updated-distances (update-points updated-distances points))))))

(def input "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.\nCupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.\nRudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.\nDonner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.\nDasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.\nBlitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.\nPrancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.\nComet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.\nVixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.")
(println (race 2503 (get-reindeer-specs input)))
(println (points-race 2503 (get-reindeer-specs input)))
