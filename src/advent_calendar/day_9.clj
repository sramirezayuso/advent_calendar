(ns advent-calendar.day-9
  (:require [clojure.string :as str]))

(defn create-distance-map
  "Create a map of distances."
  [input]
  (let [distances (str/split input #"\n")]
    (loop [i 0, distance-map {}]
      (if (>= i (count distances))
        distance-map
        (let [[origin to destination equals distance] (str/split (get distances i) #" ")]
          (recur (inc i) (assoc-in (assoc-in distance-map [(keyword origin) (keyword destination)] (Integer/parseInt distance)) [(keyword destination) (keyword origin)] (Integer/parseInt distance))))))))

(defn tsp
  "Exhaustive solution of TSP"
  [distances]
  (let [cities (keys distances)]
    (if (= 2 (count cities))
      {(nth cities 0) ((distances (nth cities 0)) (nth cities 1)) (nth cities 1) ((distances (nth cities 0)) (nth cities 1))}
      (loop [i 0, minimum-distances {}]
        (if (>= i (count cities))
          minimum-distances
          (let [city (nth cities i)]
            (recur (inc i)
                   (assoc minimum-distances city (reduce min (vals (let [cities (remove #(= city %) cities)]
                                                                (loop [j 0, updated-mins (tsp (dissoc distances city))]
                                                                  (if (>= j (count cities))
                                                                    updated-mins
                                                                    (let [new-city (nth cities j)]
                                                                      (recur (inc j) (assoc updated-mins new-city (+ (get-in distances [city new-city]) (updated-mins new-city))) )))))))))))))))

(defn inverse-tsp
  "Exhaustive solution of inverse TSP"
  [distances]
  (let [cities (keys distances)]
    (if (= 2 (count cities))
      {(nth cities 0) ((distances (nth cities 0)) (nth cities 1)) (nth cities 1) ((distances (nth cities 0)) (nth cities 1))}
      (loop [i 0, minimum-distances {}]
        (if (>= i (count cities))
          minimum-distances
          (let [city (nth cities i)]
            (recur (inc i)
                   (assoc minimum-distances city (reduce max (vals (let [cities (remove #(= city %) cities)]
                                                                     (loop [j 0, updated-mins (inverse-tsp (dissoc distances city))]
                                                                       (if (>= j (count cities))
                                                                         updated-mins
                                                                         (let [new-city (nth cities j)]
                                                                           (recur (inc j) (assoc updated-mins new-city (+ (get-in distances [city new-city]) (updated-mins new-city))) )))))))))))))))

(def input "Faerun to Tristram = 65\nFaerun to Tambi = 129\nFaerun to Norrath = 144\nFaerun to Snowdin = 71\nFaerun to Straylight = 137\nFaerun to AlphaCentauri = 3\nFaerun to Arbre = 149\nTristram to Tambi = 63\nTristram to Norrath = 4\nTristram to Snowdin = 105\nTristram to Straylight = 125\nTristram to AlphaCentauri = 55\nTristram to Arbre = 14\nTambi to Norrath = 68\nTambi to Snowdin = 52\nTambi to Straylight = 65\nTambi to AlphaCentauri = 22\nTambi to Arbre = 143\nNorrath to Snowdin = 8\nNorrath to Straylight = 23\nNorrath to AlphaCentauri = 136\nNorrath to Arbre = 115\nSnowdin to Straylight = 101\nSnowdin to AlphaCentauri = 84\nSnowdin to Arbre = 96\nStraylight to AlphaCentauri = 107\nStraylight to Arbre = 14\nAlphaCentauri to Arbre = 46")

(let [distances (create-distance-map input)]
  (println (reduce min (vals (tsp distances))))
  (println (reduce max (vals (inverse-tsp distances)))))