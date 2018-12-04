(ns aoc18.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn gen-fabric [len wid]
  (into []
        (for [x (range 0 len)]
          (into []
                (for [y (range 0 len)]
                  0)))))

(defn update-2d-matrix [mat x y val]
  (assoc mat x (assoc (nth mat x) y (+ 1 (get-in mat [x y] 0)))))

(defn set-claim [fabric-atom id x y width height]
  (doseq [cx (range x (+ x width))]
    (doseq [cy (range y (+ y height))]
      (swap! fabric-atom update-2d-matrix cx cy true))))

(defn get-duplicate-claim-count [mat x y width height]
  (let [counter (atom 0)]
    (doseq [cx (range x (+ x width))]
      (doseq [cy (range y (+ y height))]
        (when (not= 1 (get-in mat [cx cy]))
          (swap! counter inc))))
    @counter))

(defn verify-claim [mat id x y width height]
  (if (= 0 (get-duplicate-claim-count mat x y width height))
    id
    -1))

(def st "#1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2")

(defn count-intersecionts [mat]
  (reduce +
          (flatten
           (for [x (range (count mat))]
             (for [y (range (count (nth mat x)))]
               (if (< 1 (get-in mat [x y]))
                 1
                 0))))))

(defn get-solved-claim [st]
  (let [strs (-> st (str/split #"\n") (->> (map str/trim)))
        tokens (->> strs
                    (map #(map (fn [x] (Integer/parseInt x))
                               (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" %)))))
        fabric-atom (atom (gen-fabric 1000 1000))]

    (doseq [t tokens]
      (apply set-claim fabric-atom t))
    [fabric-atom tokens]))

(defn solve3a [st]
  (let [[fabric-atom _] (get-solved-claim st)]
    (count-intersecionts @fabric-atom)))

(defn solve3b [st]
  (let [[fabric-atom tokens] (get-solved-claim st)]
    (->>
     (for [t tokens]
       (apply verify-claim @fabric-atom t))

     (filter #(not= -1 %)))))

(comment
  (solve3a st)
  (solve3b st))

(defn day3a []
  (let [input (slurp (io/resource "input3a.txt"))]
    (print (solve3a input))))

(defn day3b []
  (let [input (slurp (io/resource "input3a.txt"))]
    (print (solve3b input))))

(comment
  (day3a)
  (day3b))


;; --- the same thing using java interop, very slow (multi dim array ??)


(defn gen-fabric-java [len wid]
  (to-array-2d (for [x (range len)] (for [y (range wid)] 0))))

(defn update-2d-matrix-java [mat x y val]
  (aset mat x y (inc (aget mat x y))))

(defn set-claim-java [fabric id x y width height]
  (doseq [cx (range x (+ x width))]
    (doseq [cy (range y (+ y height))]
      (update-2d-matrix-java fabric cx cy true))))

(defn get-duplicate-claim-count-java [mat x y width height]
  (let [counter (atom 0)]
    (doseq [cx (range x (+ x width))]
      (doseq [cy (range y (+ y height))]
        (when (not= 1 (aget mat cx cy))
          (swap! counter inc))))
    @counter))

(defn verify-claim-java [mat id x y width height]
  (if (= 0 (get-duplicate-claim-count-java mat x y width height))
    id
    -1))

(defn get-solved-claim-java [st]
  (let [strs (-> st (str/split #"\n") (->> (map str/trim)))
        tokens (->> strs
                    (map #(map (fn [x] (Integer/parseInt x))
                               (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" %)))))
        fabric (gen-fabric-java 1000 1000)]

    (doseq [t tokens]
      (apply set-claim-java fabric t))
    [fabric tokens]))

(defn solve3b-java [st]
  (let [[fabric tokens] (get-solved-claim-java st)]
    (->>
     (for [t tokens]
       (apply verify-claim-java fabric t))

     (filter #(not= -1 %)))))

(defn day3b-java []
  (let [input (slurp (io/resource "input3a.txt"))]
    (print (solve3b-java input))))

(comment
  (day3b-java))
