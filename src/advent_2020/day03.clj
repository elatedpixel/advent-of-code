(ns advent-2020.day03
  (:require [clojure.java.io :as io]))

(def input
  (into [] (map vec) (line-seq (io/reader (io/resource "2020/day03")))))

(defn- shape [v]
  (vector (count v) (count (first v))))

(defn- trees-on-slope [slopes]
  (let [[rows columns] (shape input)]
    (count
     (sequence
      (comp
       (take-while #(< (first %) rows))
       (map (fn [[row column]] (get-in input [row (mod column columns)])))
       (filter #{\#}))
      slopes))))

(defn- generate-slope [slope]
  (iterate #(mapv + slope %) [0 0]))

(comment
  ;; part 1
  (time (trees-on-slope (generate-slope [1 3])))
  ;; "Elapsed time: 1.28739 msecs"
  ;; => 209

  ;; part 2
  (time
   (transduce
    (comp
     (map generate-slope)
     (map trees-on-slope))
    *
    [[1 1] [1 3] [1 5] [1 7] [2 1]]))
  ;; "Elapsed time: 4.333387 msecs"
  ;; => 1574890240
                                        ;
  )
