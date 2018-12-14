(ns day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def st "Step C must be finished before step A can begin.
  Step C must be finished before step F can begin.
  Step A must be finished before step B can begin.
  Step A must be finished before step D can begin.
  Step B must be finished before step E can begin.
  Step D must be finished before step E can begin.
  Step F must be finished before step E can begin.")



(defn parse-input [st]
  (-> st
      (str/trim)
      (str/split #"\n")
      (->> (map #(re-find #"Step (.) must be finished before step (.) can begin." %))
           (map rest)
           (map (partial into []))
           )
      ))

(def tokens (parse-input st))

(defn get-empty-map [tokens]
  (reduce (fn [acc [a _]]
            (assoc acc a []))
          (sorted-map)
          tokens))

(defn build-adjacency-list [tokens]
  (reduce (fn [acc [a b]]
            (update-in acc [b] conj a))
          (get-empty-map tokens)
          tokens))

(defn remove-node [graph [node _]]
  ;;(print graph node)
  (->> (dissoc graph node)
       (reduce (fn [acc [k v]]
                 (assoc acc k (remove #(= node %) v)))
               (sorted-map))
))



(defn solve6a [st]
  (let [tokens (parse-input st)
        al (build-adjacency-list tokens)
        ]
    (loop [graph al
           ans []]
      ;;(println ans)
      (def g graph)
      (let [nodes-remove (filter (fn [[k v]] (= (count v) 0)) graph)]
        ;;(print nodes-remove)
        (if (not-empty graph)
          (recur (remove-node graph (first nodes-remove))
                 ;(reduce #(remove-node %1 %2) graph nodes-remove)
                 (into [] (concat ans (nth (first nodes-remove) 0))))
          (println (str/join ans)))))
    ))

;;(solve6a st)

(defn day7a []
  (let [input (str/trim (slurp (io/resource "input7.txt")))]
    (solve6a input)))


(defn find-best-worker [workers]
  (sort-by (fn [[k v] v]) workers))

(defn remove-worker [worker time]
  (reduce (fn [acc [k v]]
            (assoc acc (- k time) v))
          (sorted-map)
          worker))

(defn get-char-value [[c]]
  (+ 1 (- (int c) (int \A))))

(defn solve7b [st]
  (let [tokens (parse-input st)
        al (build-adjacency-list tokens)
        num-workers 2
        ]
    (loop [graph al
           total-time 0
           workers (sorted-map) ;; time left -id 
           check-graph true
           nodes-handled #{}
           ]
      ;;(println workers)
      (if check-graph
        (let [workers-left (- num-workers (count workers))
              nodes-remove (take workers-left (filter (comp not nodes-handled first)(filter (fn [[k v]] (= (count v) 0)) graph)) )]
          ;;(println nodes-remove)
          (if (not-empty graph)
            (recur graph
                   total-time
                   (reduce (fn [acc [k v]] (assoc acc k v)) workers (map (fn [[x _]] [(get-char-value x) x]) nodes-remove))
                   false
                   (into #{} (concat nodes-handled nodes-remove))
                   )
            (println total-time)))
        (if (not-empty workers)
          (let [[time-used id] (first workers)]
            (recur (remove-node graph [id :not-needed])
                   (+ total-time time-used)
                   (remove-worker (rest workers) time-used)
                   true
                   nodes-handled))

          )))
    ))

;;(solve7b st)

(defn day7b []
  (let [input (str/trim (slurp (io/resource "input7.txt")))]
    (solve7b input)))

(comment
  (day7a)
  (day7b))

