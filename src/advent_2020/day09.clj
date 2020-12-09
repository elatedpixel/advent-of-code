(ns advent-2020.day09
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(def test-data
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn- parse-input [s] (read-string (str "[" s "]")))

(def input-data
  (parse-input (slurp (io/resource "2020/day09"))))

(defn- sum [ns] (reduce + ns))

(defn- valid-sums [ns]
  (into #{} (map sum) (combo/combinations ns 2)))

(defn get-invalid-sums [preamble data]
  (for [[i n] (vec (drop preamble (map-indexed vector data)))
       :when (nil? ((valid-sums (subvec data (- i preamble) i)) n))]
   [i n]))

;; part 1
(comment
  (time (println (get-invalid-sums 25 input-data)))
  ;; ([561 70639851])
  ;; "Elapsed time: 462.728725 msecs"
  )

(defn search-for-contiguous-sum [data n]
  (loop [data data]
    (let [sums (take-while (partial >= n) (reductions + data))]
      (if (= n (last sums))
        (->> data
             (take (count sums))
             ((juxt (partial apply min) (partial apply max)))
             (reduce + 0))
        (recur (rest data))))))

;; part 2
(comment
  (time
   (println
    (search-for-contiguous-sum input-data 70639851)))
  ;; 8249240
  ;; "Elapsed time: 32.990167 msecs"
  )
