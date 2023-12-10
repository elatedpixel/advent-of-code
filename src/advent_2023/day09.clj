(ns advent-2023.day09 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day09"))))
(def sample (str/trim (slurp (io/resource "2023/day09.sample"))))

(defn- parse-history
  [s]
  (map parse-long (re-seq #"[\-\d]+" s)))

(defn- parse-report
  [s]
  (map parse-history (str/split-lines (str/trim s))))

(defn- difference [history]
  (map - (rest history) history))

(defn- differences [history]
  (take-while
   (partial some (complement zero?))
   (iterate difference history)))

(comment
  (differences [10 13 16 21 30 45])
  ;; => ([10 13 16 21 30 45] (3 3 5 9 15) (0 2 4 6) (2 2 2))
  ;;                      ^            ^         ^       ^
  ;; the last element of each list sum to the next number in the series
  )

(defn- predict [history]
  (transduce (map last) + (differences history)))

(differences [10 11 3 -24 -79 -164 -268 -361 -388 -263 137 978 2475 4898 8578 13913 21374 31511 44959 62444 84789])
;; => 112920

(defn- extrapolated-sum
  [report]
  (transduce
   (map predict)
   +
   report))

(defn- part-1
  [input]
  (extrapolated-sum (parse-report input)))

(test/deftest test-part-1
  (test/is (= 114 (part-1 sample)))
  (test/is (= 1882395907 (part-1 input))))

(defn- part-2
  [input]
  (extrapolated-sum (map reverse (parse-report input))))

(test/deftest test-part-2
  (test/is (= 2 (part-2 sample)))
  (test/is (= 1005 (part-2 input))))

