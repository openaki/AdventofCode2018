(ns day9)

(defn cycle-anticlock [^java.util.LinkedList lst count]
  (doseq [x (range count)]
    (let [elem (.getFirst lst)]
      (.removeFirst lst)
      (.addLast lst elem))))

(defn cycle-clock [^java.util.LinkedList lst count]
  (doseq [x (range count)]
    (let [elem (.getLast lst)]
      (.removeLast lst)
      (.addFirst lst elem))))

(defn solve9a [players marbles]
  (let [circle (java.util.LinkedList. )
        scores (atom {})]
    (doseq [x (range players)]
      (swap! scores assoc x 0))
    (.addLast circle 0)
    (doseq [x (range 1 (inc marbles))]
      (if (= (mod x 23) 0)
        (do
          (cycle-clock circle 7)
          (let [elem (.getLast circle)]
            (swap! scores update (mod x players) + x elem)
            (.removeLast circle))
          (cycle-anticlock circle 1))
        (do
          (cycle-anticlock circle 1)
          (.addLast circle x))))

    (apply max (vals @scores))))

(solve9a 426 7205800)



