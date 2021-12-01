(ns advent-2021.day01
  (:require [advent.core :refer (load-input)]))

(def input
  (map #(Integer/parseInt %) (load-input 2021 1)))

;; part 1
(comment
  (transduce
    (keep (fn [[a b]] (when (< a b) 1)))
    +
    (partition 2 1 input));; => 1709
;
  )

;; part 2
(comment
  (->> (partition 3 1 input)
       (map #(reduce + %))
       (partition 2 1)
       (reduce (fn [x [a b]] (+ x (if (< a b) 1 0))) 0));; => 1761
;
  )
