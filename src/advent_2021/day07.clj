(ns advent-2021.day07
  (:require [advent.core :refer (load-input string->sexpression)]))

(def input (string->sexpression (first (load-input 2021 7))))

(defn sum [xs] (reduce +' xs))

(defn delta [x]
  (fn [y] (Math/abs (- x y))))

(defn gold-fuel-cost [x]
  (/ (* x (inc x)) 2))

(defn align [xs fuel-cost]
  (apply min-key second
        (for [i (range 1000)]
          (list i (sum (map (comp fuel-cost (delta i)) xs))))))

(align input identity)
;; => (361 354129)

(align input gold-fuel-cost)
;; => (494 98905973)
