(ns day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def st "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn parse-input [st] (into [] (map #(Integer/parseInt %) (str/split st #" "))))

(defn iterate-node [nodes input start-loc]
  (reduce (fn [[loc nm] c])))

(defn add-to-stack [stack input]
  (let [[nodes metadata]  (into [] (take 2 input))]
    (doseq [x (range metadata)]
      (swap! stack conj [:m x]))
    (doseq [x (range nodes)]
      (swap! stack conj [:n x]))))

(defn solve8a [input]
  (let [stack (atom [])
        ans (atom 0)
        string-counter (atom 2)]
    (add-to-stack stack input)
    (while (not-empty @stack)
      (let [item (last @stack)]
        (swap! stack pop)
        (condp #(= %1 (first %2) ) item
          :m (do
               (swap! ans + (nth input @string-counter))
               (swap! string-counter inc))
          :n (do
               (add-to-stack stack (drop @string-counter input))
               (swap! string-counter + 2)))))
    @ans))

(solve8a (parse-input st))

(defn solve8b [input]
  (let [stack (atom [])
        ans (atom 0)
        string-counter (atom 2)
        last-metadata (atom [])
        last-nodes (atom [])
        ]
    (add-to-stack stack input)
    (while (not-empty @stack)
      (let [item (last @stack)]
        (swap! stack pop)
        (condp = (first item)
          :m (do
               (swap! last-metadata conj (nth input @string-counter))
               (swap! string-counter inc))
          :n (do
               (add-to-stack stack (drop @string-counter input))
               (swap! string-counter + 2)))))
    @ans))

(defn day8a []
  (let [input (parse-input (str/trim (slurp (io/resource "input8.txt"))))]
    (solve8a input)))


(defn get-node-score [scores metadata]
  ;;(println "--->>>" scores metadata)
  (into []
        (for [x metadata]
          (do
            (if (and (<= x (count scores)) (> x 0))
              (nth scores (- x 1))
              0))
          )))

(defn solve8b [input index]
  (let [[nodes metadata] (take 2 (drop index input))
        string-counter (atom (+ index 2))
        scores (atom []) 
        ]

    (doseq [x (range nodes)]
      (let [[score sc] (solve8b input @string-counter)]
        ;;(println score)
        (swap! string-counter + sc)
        (swap! scores conj score)
        ))
    ;;(println @scores @string-counter (= 0 nodes) (count input))
    (let [metdatas (take metadata (drop @string-counter input))]
      (if (= 0 nodes)
        [(apply + metdatas) (+ metadata (- @string-counter index))]
        [
         (apply + (get-node-score @scores metdatas))
         (+ metadata (- @string-counter index))]
        ))
    ))

(solve8b (parse-input st) 0)

(defn day8b []
  (let [input (parse-input (str/trim (slurp (io/resource "input8.txt"))))]
    (solve8b input 0)))

(day8b)

