(ns advent-calendar.day-4
  (:import (java.security MessageDigest)))

(defn md5
  "Finds the MD5 hash of a given string"
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn find-md5-integer
  "Finds the first integer such that (md5 (str input integer)) starts with the given string."
  [input startsWith]
  (loop [i 0]
    (if (.startsWith (md5 (str input i)) startsWith)
      i
      (recur (inc i)))))

(def input "bgvyzdsv")
(println (find-md5-integer input "00000"))
(println (find-md5-integer input "000000"))