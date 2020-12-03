(ns advent-2020.day03
  (:require [clojure.java.io :as io]))

(def input
  (into [] (map vec) (line-seq (io/reader (io/resource "2020/day03")))))

(defn- shape [v]
  (vector (count v) (count (first v))))

(defn- trees-on-slope [slope]
  (let [[rows columns] (shape input)]
    (count
     (sequence
      (comp
       (take-while #(< (first %) rows))
       (map (fn [[row column]] (vector row (mod column columns))))
       (map (fn [coordinate] (get-in input coordinate)))
       (filter #{\#}))
      slope))))

(defn- generate-slope [slope]
  (iterate #(mapv + slope %) [0 0]))

(comment
  ;; part 1
  (trees-on-slope (generate-slope [1 3]))
  ;; => 209

  ;; part 2
  (transduce
   (comp
    (map generate-slope)
    (map trees-on-slope))
   *
   [[1 1] [1 3] [1 5] [1 7] [2 1]])
  ;; => 1574890240
                                        ;
  )
