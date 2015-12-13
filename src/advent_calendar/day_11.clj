(ns advent-calendar.day-11)

(defn next-string
  "Increments a string and returns the new string."
  [string]
  (if (= string "z")
    "a"
    (let [idx (dec (count string))]
      (if (= (get string idx) \z)
        (str (next-string (subs string 0 idx )) \a)
        (str (subs string 0 idx ) (char (inc (int (get string idx)))))))))

(defn has-invalid-character
  "Checks if a string contains i, l or o."
  [string]
  (not (boolean (re-matches #"^[^ilo]+$" string))))

(defn has-double-character
  "Checks if it has a duplicated character."
  [string]
  (> (count (re-seq #"(.)\1" string)) 1))

(defn has-consecutive-character
  "Checks if it has three consecutive characters."
  [string]
  (let [limit (- (count string) 3)]
    (loop [i 0]
      (cond
        (> i limit) false
        (= (int (get string i)) (dec (int (get string (inc i)))) (- (int (get string (+ i 2))) 2)) true
        :else (recur (inc i))))))

(defn valid-password?
  "Checks if string is a valid password."
  [string]
  (and (has-double-character string) (has-consecutive-character string) (not (has-invalid-character string))))

(defn next-password
  "Returns the next password based on a previous one."
  [password]
  (loop [string (next-string password)]
    (if (valid-password? string)
      string
      (recur (next-string string)))))

(def input "hepxcrrq")
(println (next-password input))
(println (next-password (next-password input)))
