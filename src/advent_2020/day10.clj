(ns advent-2020.day10
  (:require [clojure.java.io :as io]))

(def sample-input-1 [16 10 15 5 1 11 7 19 6 12 4])
(def sample-input-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def input (read-string (str "[" (slurp (io/resource "2020/day10")) "]")))

(defn add-device-adapter [coll] (conj coll (+ 3 (apply max coll))))
(defn add-charging-adapter [coll] (conj coll 0))
(def with-external-adapters (comp sort add-charging-adapter add-device-adapter))

(defn adapter-chain-deltas [data]
  (frequencies (map - (rest data) data)))

(assert (= {1 7 3 5} (adapter-chain-deltas (with-external-adapters sample-input-1))))
(assert (= {1 22 3 10} (adapter-chain-deltas (with-external-adapters sample-input-2))))

;; part 1
(comment
  (time
   (let [deltas (adapter-chain-deltas (sort (with-external-adapters input)))]
     (println deltas (* (deltas 1) (deltas 3)))))
  ;; {1 70, 3 31} 2170
  ;; "Elapsed time: 0.494073 msecs"
  )

(defn graph [coll]
  (let [data (into [] (sort coll))]
    (into {} (for [[i n] (mapv vector (range) data)]
               [n (filter #(< 0 (- % n) 4) (subvec data (inc i)))]))))

(defn backtrack [m index goal]
  (if (= goal index)
    1
    (reduce + (for [neighbor (m index)] (backtrack m neighbor goal)))))

(assert (= 8 (backtrack (graph (with-external-adapters sample-input-1)) 0 22)))
(assert (= 19208 (backtrack (graph (with-external-adapters sample-input-2)) 0 52)))

(comment
  (time
   (println
    (with-redefs [backtrack (memoize backtrack)]
      (backtrack (graph (with-external-adapters input)) 0 163))))
  ;; 24803586664192
  ;; "Elapsed time: 3.22088 msecs"
  )
