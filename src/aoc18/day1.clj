(ns aoc18.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(comment
  (def st "+1\n -2\n +3\n +1")
  (def st "+3\n +3\n +4\n -2\n -4")
  (def st "-6\n +3\n +8\n +5\n -6")
  (def st "+7\n +7\n -2\n -7\n -4")
  )

(defn solve1a [st]
  (-> st
      (str/split #"\n")
      (->> (map #(Integer/parseInt %))
           (reduce +))))

(defn calc-duplicate-freq [nums]
  (let [numLen (count nums)]
    (loop [currentSum 0 seenMap #{}
           currentIndex 0]
      (let [curNum (nth nums currentIndex)
            newSum (+ currentSum curNum)]
        (if (seenMap newSum)
          newSum
          (recur newSum
                 (conj seenMap newSum )
                 (mod (+ 1 currentIndex) numLen)))))))


(defn solve2a [st]
  (-> st
      (str/split #"\n")
      (->> (map str/trim)
           (map #(Integer/parseInt %))
           (calc-duplicate-freq))))


(defn day1a []
  (let [input (slurp (io/resource "input1a.txt"))]
    (print (solve1a input))))

(defn day2a []
  (let [input (slurp (io/resource "input1a.txt"))]
    (print (solve2a input))))


(comment
  (day1a)
  (day2a))

