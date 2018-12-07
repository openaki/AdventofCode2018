(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def st "dabAcCaCBAcCcaDA")

(defn collide? [a b]
  (and a b (not= a b) (= (Character/toUpperCase a) (Character/toUpperCase b))))

(defn reduce-fn [acc x]
  (if (collide? x (peek acc))
    (pop acc)
    (conj acc x)))

(defn solve5a [st]
  (count (reduce reduce-fn [] st)))

(defn day5a []
  (let [input (str/trim (slurp (io/resource "input5a.txt")))]
    (print (solve5a input))))

(defn solve5b [data]
  (apply min (pmap (fn [char]
                     (let [input (remove #(or (= % char)
                                              (= % (Character/toUpperCase char))) data)]
                       (solve5a input)))
                  "abcdefghijklmnopqrstuvwxyz")))

(defn day5b []
  (let [input (str/trim (slurp (io/resource "input5a.txt")))]
    (print (solve5b input))))

(comment 
  (day5a)
  (day5b))

