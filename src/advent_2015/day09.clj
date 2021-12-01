(ns advent-2015.day09
  (:require
   [clojure.java.io :as io]
   [clojure.math.combinatorics :refer (permutations)]))

(def input (slurp (io/resource "2015/day09")))

(defn distance-map [input]
  (into {} (for [[_ from to distance] (re-seq #"(\w+) to (\w+) = (\d+)" input)]
             [#{from to} (Integer/parseInt distance)])))

(let [m (distance-map input)]
  (apply max
         (sequence
          (comp
           (map (partial partition 2 1))
           (map #(map set %))
           (map #(map m %))
           (map #(reduce + %)))
          (permutations (set (mapcat (juxt first second) (keys (distance-map input))))))))
