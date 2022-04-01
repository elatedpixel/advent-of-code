(ns advent-2021.day15
  (:require
   [clojure.java.io :as io]
   [clojure.data.priority-map :refer (priority-map)]
   [advent.core :as c]))

(def input
  (mapv (comp (partial mapv (comp read-string str)) vec)
        (c/load-input 2021 15)))

(defn neighbors [coord]
  (mapv (partial map + coord) '((-1 0) (1 0) (0 -1) (0 1))))

(defn grid [input]
  (into {} (for [i (range (count input))
                 j (range (count (input i)))]
             [[i j] (get-in input [i j])])))

(defn graph [grid]
  (into
   {}
   (map (fn [[[i j] v]]
          [[i j]
           (into {}
                 (comp (map (juxt identity grid))
                       (filter (comp some? second)))
                 (neighbors [i j]))]))
   grid))

(def g (graph (grid input)))

(comment
  (time ((c/dijkstra g [0 0]) [99 99]))
  ;; => 415
  ;; "Elapsed time: 110.04832 msecs"
  )

(defn embiggen [input]
  (into {} (for [i    (range (count input))
                 j    (range (count (input i)))
                 k    (range 5)
                 l    (range 5)
                 :let [y (+ i (* 100 k))
                       x (+ j (* 100 l))
                       distance (+ k l)
                       cost (+ (get-in input [i j]) distance)
                       adjusted-cost (if (> cost 9) (mod cost 9) cost)]]
             [[y x] adjusted-cost])))

(def G (graph (embiggen input)))

(comment
  (time ((c/dijkstra G [0 0]) [499 499]))
  ;; => 2864
  ;; "Elapsed time: 2644.721516 msecs"
  )
