(ns advent-calendar.day-10
  (:import (java.util.regex Pattern)))

(defn look-and-say
  "Apply look-and-say"
  [input i iter]
  (if (>= i iter)
    input
    (clojure.string/replace (look-and-say input (inc i) iter) #"((.)\2*)" #(let [group (get %1 1)]
                                                                            (str (count group) (get group 0))))))

(def input "3113322113")
(println (count (look-and-say input 0 40)))
(println (count (look-and-say input 0 50)))
