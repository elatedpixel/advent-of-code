(ns advent-2023.day01
  (:require
   [advent.core :as c]
   [clojure.string :as str]
   [clojure.test :as test]))

(def sample "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

(def sample-2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def input (c/load-input 2023 1))

;; don't judge me, it was midnight
(def digit-lookup
  {"1"     1
   "2"     2
   "3"     3
   "4"     4
   "5"     5
   "6"     6
   "7"     7
   "8"     8
   "9"     9
   "one"   1
   "two"   2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9})

(defn search [calibration-value]
  (filter (partial str/includes? calibration-value) (keys digit-lookup)))

(defn leftmost [s]
  (apply min-key (partial str/index-of s) (search s)))

(defn rightmost [s]
  (apply max-key (partial str/last-index-of s) (search s)))

(defn parse-calibration-value [calibration-value]
  (Integer/parseInt (str (digit-lookup (leftmost calibration-value))
                         (digit-lookup (rightmost calibration-value)))))

(defn part-1 [input]
  (transduce (map (comp parse-calibration-value
                        (partial re-seq #"\d"))) + input))

(defn part-2 [input]
  (transduce (map parse-calibration-value) + input))

(test/is (= 142 (part-1 (str/split-lines (str/trim sample)))))

(test/is (= 281 (part-2 (str/split-lines (str/trim sample-2)))))

(test/is (= 55130 (part-1 input)))

(test/is (= 54985 (part-2 input)))
