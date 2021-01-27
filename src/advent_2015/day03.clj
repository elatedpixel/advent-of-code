(ns advent-2015.day03
  (:require [clojure.java.io :as io]))

(defn move [[x y] direction]
  (case direction
    \> [(inc x) y]
    \v [x (inc y)]
    \< [(dec x) y]
    \^ [x (dec y)]))

(defn part1 [input]
  (count (distinct (reductions move [0 0] input))))

(time (println "day03 part1" (part1 (slurp (io/resource "2015/day03")))))

(defn part2 [input]
  (count (into #{}
               (concat (reductions move [0 0] (flatten (partition 1 2 input)))
                       (reductions move [0 0] (flatten (partition 1 2 (rest input))))))))

(time (println "day03 part2" (part2 (slurp (io/resource "2015/day03")))))
