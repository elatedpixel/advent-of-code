(ns advent-2021.day01
  (:require [advent.core :refer (load-input)]))

(def input
  (map #(Integer/parseInt %) (load-input 2021 1)))

(defn count-by [pred coll]
  (reduce +
          (map (comp {false 0 true 1} pred)
               coll
               (rest coll))))

;; part 1
(count-by < input)
;; => 1709

;; part 2
(let [sliding-sum (map + input (rest input) (rest (rest input)))]
  (count-by < sliding-sum))
;; => 1761
