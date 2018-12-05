(ns day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def st "
  [1518-11-01 00:00] Guard #10 begins shift
  [1518-11-01 00:05] falls asleep
  [1518-11-01 00:25] wakes up
  [1518-11-01 00:30] falls asleep
  [1518-11-01 00:55] wakes up
  [1518-11-01 23:58] Guard #99 begins shift
  [1518-11-02 00:40] falls asleep
  [1518-11-02 00:50] wakes up
  [1518-11-03 00:05] Guard #10 begins shift
  [1518-11-03 00:24] falls asleep
  [1518-11-03 00:29] wakes up
  [1518-11-04 00:02] Guard #99 begins shift
  [1518-11-04 00:36] falls asleep
  [1518-11-04 00:46] wakes up
  [1518-11-05 00:03] Guard #99 begins shift
  [1518-11-05 00:45] falls asleep
  [1518-11-05 00:55] wakes up")

(defn parse-line-token [[_ month day hour min st]]
  (let [g (re-find #"Guard #(\d+) begins shift" st)
        s (re-find #"falls asleep" st)
        w (re-find #"wakes up" st)
        pi (fn [x] (Integer/parseInt x))]
    (merge {:month (pi month) :day (pi day) :hour (pi hour) :min (pi min)}
           (cond
             g {:id (nth g 1) :type :guard}
             s {:type :sleep}
             w {:type :wakes}))))

(defn get-min [h m]
  (if (== h 11)
    0
    m))

(defn fill-val [start end val]
  (into []
        (for [x (range start end)] {:min x :val val})))

(defn get-fill-val [l id]
  (if (= l :sleep)
    id
    0))

(defn finalize [v id l]
  (if (or (= l :wakes) (= 0 (count v)))
    v
    (into []
          (if (> 60 (count v))
            (concat v (fill-val (count v) 60 (get-fill-val l id)))
            v))))

(defn join-v [a b]
  (into [] (concat a b)))

(defn- reduce-fn [acc {:keys [month day hour min type id] :or {id nil}}]
  (let [tm (get-min hour min)]
    (condp = type
      :guard (assoc acc :id id :curr-min tm :final (into [] (conj (:final acc) (finalize (:curr acc) (:id acc) (:last acc)))) :curr [])
      :sleep (assoc acc :last :sleep :curr-min min)
      :wakes (assoc acc :last :wakes :curr-min min :curr (join-v (:curr acc) (fill-val (:curr-min acc) min (:id acc)))))))

(defn get-final-vec [st]
  (->> (str/split st #"\n")
       (filter #(not= "" %))
       (sort)
       (map #(re-find #"\[\d+-(\d+)-(\d+) (\d+):(\d+)\] (.*)" %))
       (map parse-line-token)
       (reduce reduce-fn {})
       ((fn [x] (into [] (conj (:final x) (:curr x)))))))

(defn get-id-per-day [d]
  (flatten (map :val d)))

(defn get-min-per-day [d id]
  (into []
        (map :min (filter #(= id (:val %)) d))))

(defn solve4a [st]
  (let [fv (get-final-vec st)
        ids (frequencies (flatten (map get-id-per-day fv)))
        max-days (last (sort (vals ids)))
        id (first (keep #(when (= (val %) max-days) (key %)) ids))
        mins-per-day (map #(get-min-per-day % id) fv)
        mins (frequencies (flatten mins-per-day))
        max-mins (last (sort (vals mins)))
        min (first (keep #(when (= (val %) max-mins) (key %)) mins))]

    (* (Integer/parseInt id) min)))


(defn day4a []
  (let [input (slurp (io/resource "input4a.txt"))]
    (print (solve4a input))))


(defn get-max-min-guard-single-day [min day]
  (map :val (filter #(= (:min %) min) day)))

(defn get-max-min-guard-all-days [min days]
  (let [mins (frequencies (flatten (map #(get-max-min-guard-single-day min %) days)))
        max-mins-t (last (sort (vals mins)))
        max-mins (if max-mins-t max-mins-t 0)
        m (first (keep #(when (= (val %) max-mins) (key %)) mins))]
    [min m max-mins]))

(defn solve4b [st]
  (let [fv (get-final-vec st)
        id-mins (for [d (range 0 60)] (get-max-min-guard-all-days d fv))
        [min id count] (last (sort-by #(nth % 2) id-mins))]
    (* min (Integer/parseInt id))))

(comment
  (solve4a st)
  (solve4b st)
  )

(defn day4b []
  (let [input (slurp (io/resource "input4a.txt"))]
    (print (solve4b input))))

(comment
  (day4a)
  (day4b)
  )
