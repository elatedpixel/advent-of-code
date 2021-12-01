(ns advent-2021.day01
  (:require [advent.core :refer (load-input)]
            [criterium.core :as criterium]))

(def input (map #(Integer/parseInt %) (load-input 2021 1)))

;; part 1
((frequencies (map < input (rest input))) true)
;; => 1709

;; part 2
(criterium/quick-bench ((frequencies (map < input (drop 3 input))) true))
;; => 1761
;; Evaluation count : 2268 in 6 samples of 378 calls.
;;              Execution time mean : 265.151837 µs
;;     Execution time std-deviation : 1.272413 µs
;;    Execution time lower quantile : 264.027103 µs ( 2.5%)
;;    Execution time upper quantile : 267.245155 µs (97.5%)
;;                    Overhead used : 3.710576 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;;  low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
