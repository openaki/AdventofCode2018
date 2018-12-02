(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def st "abcdef
  bababc
  abbcbe
  abcccd
  aabcdd
  abcdee
  ababab
  ")

(defn- solve2a [st]
  (let [frequencies
        (-> st
            (str/split #"\n")
            (->> (map str/trim)
                 (map frequencies)
                 (map vals)
                 (map frequencies)))
        twos (count (filter identity (map #(contains? % 2)frequencies)))
        thress (count (filter identity (map #(contains? % 3)frequencies)))]
    (* twos thress)))

(defn- distance [s1 s2]
  (count
   (filter identity
           (map not= s1 s2))))

(defn- solve2b [st]
  (let [strs (-> st (str/split #"\n") (->> (map str/trim)))]
    (for [s1 strs
          s2 strs
          :when (= 1 (distance s1 s2))]
      (do
        [s1 s2]))))

(comment
  (solve2a st)
  (solve2b st))

(defn day2a []
  (let [input (slurp (io/resource "input2a.txt"))]
    (print (solve2b input))))

(defn day2b []
  (let [input (slurp (io/resource "input2a.txt"))]
    (print (solve2b input))))

(comment
  (day2a)
  (day2b))
