(ns advent-2021.day06
  (:require [advent.core :refer (load-input string->sexpression)]
            [criterium.core :refer (quick-bench)]))

(def input (string->sexpression (first (load-input 2021 6))))

(defn age [fish day]
  (if (zero? day)
    (reduced (reduce + (vals fish)))
    (let [fish' (into {} (for [[k v] fish] [(dec k) v]))]
      (if-let [spawned (fish' -1)]
        (-> fish'
            (update 6 (fnil + 0) spawned)
            (update 8 (fnil + 0) spawned)
            (dissoc -1))
        fish'))))

(defn fish-produced-by-day [fish day]
  (reduce age (frequencies fish) (reverse (range (inc day)))))

;; silver
(quick-bench (fish-produced-by-day input 80))
;; => 351188
;; Evaluation count : 1896 in 6 samples of 316 calls.
;;              Execution time mean : 318.419057 µs
;;     Execution time std-deviation : 4.189077 µs
;;    Execution time lower quantile : 315.631896 µs ( 2.5%)
;;    Execution time upper quantile : 325.400332 µs (97.5%)
;;                    Overhead used : 4.086691 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;;  low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

;; gold
(quick-bench (fish-produced-by-day input 256))
;; => 1595779846729
;; Found 1 outliers in 6 samples (16.6667 %)
;;  low-severe	 1 (16.6667 %)
;;  Variance from outliers : 81.3502 % Variance is severely inflated by outliers
;; Evaluation count : 672 in 6 samples of 112 calls.
;;              Execution time mean : 898.095746 µs
;;     Execution time std-deviation : 8.709001 µs
;;    Execution time lower quantile : 891.187348 µs ( 2.5%)
;;    Execution time upper quantile : 907.899627 µs (97.5%)
;;                    Overhead used : 4.086691 ns
