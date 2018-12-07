(ns day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def st "1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9")

(defn get-coordinate-list [st]
  (-> st
      (str/split #"\n")
      (->> (map #(re-find #"(\d+), (\d+)" %))
           (map rest)
           (map #(map (fn [x] (Integer/parseInt x)) %) )
           (map #(into [] %)))))

(def input (str/trim (slurp (io/resource "input6.txt"))))

(def coordinates  (get-coordinate-list input))

(def max-x (apply max (map #(nth % 0) coordinates)))

(def max-y (apply max (map #(nth % 1) coordinates)))

(def grid
  (for [x (range 0 (inc max-x))
        y (range 0 (inc max-y))]
    [x y]))

(defn manhattan [[^long x ^long y] [^long x1 ^long y1]]
  (+ (Math/abs (- x x1)) ( Math/abs (- y y1))))

(defn closest-to [gp]
  (let [distances-from-all-pts (map (fn [p] [(manhattan gp p) p]) coordinates)
        closest (sort-by first distances-from-all-pts)]
    (when (not= (first (first closest))
                (first (second closest)))
      [(second (first closest)) gp])))

(defn infinite-area? [area]
  (some (fn [[^long x ^long y]]
          (or (zero? x)
              (zero? y)
              (>= x ^long max-x)
              (>= y ^long max-y))) area))

(defn solve-1 []
  (let [groups (group-by first (keep closest-to grid))]
    (apply max (keep #(when (not (infinite-area? (map second %)))
                        (count %)) (vals groups)))))


(defn solve-2 []
  (->>
    (for [p grid] (reduce + (map #(manhattan p %) coordinates)))
    (filter #(< % 10000))
    count))

(comment
  (solve-1)

  (solve-2)

  )


