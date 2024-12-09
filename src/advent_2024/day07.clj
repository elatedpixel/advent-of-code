(ns advent-2024.day07
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def ^:private sample0
  (line-seq (io/reader (io/resource "2024/day07.sample0"))))

(def ^:private puzzle
  (line-seq (io/reader (io/resource "2024/day07.txt"))))

(defn- parse
  [input]
  (into
   []
   (pmap
    (fn [equation]
      (let [[test-value & operands]
            (map parse-long (re-seq #"\d+" equation))]
        [test-value (vec operands)]))
    input)))

(defn- generate-equations
  [operators [x & xs]]
  (if (empty? xs)
    [x]
    (for [operator operators
          results  (generate-equations operators xs)]
      (operator x results))))

(defn equation-rf
  [r [test-value operands]]
  (+ r (if (some (partial = test-value)
                 (generate-equations (reverse operands)))
         test-value
         0)))

(defn part-1
  "Try to find combination of operators from #{+ *} that complete the equations (left-to-right no operator precedence)."
  [equations]
  (let [operators [#'+ #'*]]
    (reduce
     (fn [r [test-value operands]]
       (+ r (if (some (partial = test-value)
                      (generate-equations operators (reverse operands)))
              test-value
              0)))
     0
     equations)))

(t/deftest test-part-1
  (t/is (= 3479 (part-1 (parse sample0)))))

(defn- ||
  "Concatenation with shenanigans because we process the operands recursively so
  to enforce left-to-right order we reverse it, so we have to swap (x y) here
  too. /sigh"
  ([] 0)
  ([x] (cast Number x))
  ([x y] (parse-long (str y x)))
  ([x y & more] (reduce || (|| x y) more)))

(defn part-2
  "Try to find combination of operators from #{+ * ||} that complete the equations (left-to-right no operator precedence)."
  [equations]
  (let [operators [#'+ #'* #'||]]
    (reduce
     (fn [r [test-value operands]]
       (+ r (if (some (partial = test-value)
                      (generate-equations operators (reverse operands)))
              test-value
              0)))
     0
     equations)))

(t/deftest test-part-2
  (t/is (= 11387 (part-2 (parse sample0)))))

(comment
  (re-seq #"\d+" (first sample0))
  ;; => ("190" "10" "19")

  (parse sample0)
  ;; => [[190 [10 19]]
  ;;     [3267 [81 40 27]]
  ;;     [83 [17 5]]
  ;;     [156 [15 6]]
  ;;     [7290 [6 8 6 15]]
  ;;     [161011 [16 10 13]]
  ;;     [192 [17 8 14]]
  ;;     [21037 [9 7 18 13]]
  ;;     [292 [11 6 16 20]]]

  ;; hmmm, implementation is evaluating right-to-left
  (generate-equations [#'+ #'*] [81 40 27])
  ;; => (148 1161 5427 87480)

  ;; need to reverse the input
  (generate-equations [#'+ #'*] (reverse [81 40 27]))
  ;; => (148 3267 3267 87480)

  (generate-equations [#'+ #'* #'||] (reverse [6 8 6 15]))
  ;; => (35
  ;;     69
  ;;     89
  ;;     99
  ;;     303
  ;;     423
  ;;     161
  ;;     501
  ;;     701
  ;;     300
  ;;     810
  ;;     1110
  ;;     1260
  ;;     4320
  ;;     6120
  ;;     2190
  ;;     7290
  ;;     10290
  ;;     2015
  ;;     5415
  ;;     7415
  ;;     8415
  ;;     28815
  ;;     40815
  ;;     14615
  ;;     48615
  ;;     68615)
;
  )

(defn -main []
  (println (str "Day 7 Part 1: " (part-1 (parse puzzle))))
  (println (str "Day 7 Part 2: " (part-2 (parse puzzle)))))
