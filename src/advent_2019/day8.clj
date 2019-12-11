(ns advent-2019.day8
  (:require [clojure.test :as t]
            [advent-2019.core :refer [read-lines]]))

(def input (first (read-lines "2019/day8.txt")))

;; part 1
(->> input
     (partition (* 25 6))
     (map frequencies)
     (apply min-key #(% \0))
     ((fn [m] (* (m \1) (m \2)))))

;; part 2
(->> input
     (partition (* 25 6))
     (reduce (partial map (fn [a b] (if (= \2 a) b a))))
     (partition 25)
     (map (partial map #(if (= \0 %) " " %)))
     (map println))
