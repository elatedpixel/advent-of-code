(ns advent-2023.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math :refer [sqrt ceil floor]]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day06"))))
(def sample (str/trim (slurp (io/resource "2023/day06.sample"))))

(defn- parse [input]
  (->> input
       str/split-lines
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))
       (apply map vector)))

(defn- parse-2 [input]
  (->> input
       str/split-lines
       (map (fn [line] (parse-long (apply str (re-seq #"\d+" line)))))
       vector))

(defn- solve-quadratic [a b c]
  (let [sq (sqrt (- (* b b) (* 4 a c)))]
    (list (/ (* (+ (- b) sq)) (* 2 a))
          (/ (* (- (- b) sq)) (* 2 a)))))

(defn- records [[time distance]]
  (let [[a b] (solve-quadratic -1 time (- distance))]
    (int (inc (- (floor b) (ceil a))))))

(defn part-1 [input]
  (transduce (map records)
             *
             (parse input)))

(test/deftest test-part-1
  (test/is (= 288 (part-1 sample)))
  (test/is (= 2269432 (part-1 input))))

(defn part-2 [input]
  (transduce (map records)
             *
             (parse-2 input)))

(test/deftest test-part-2
  (test/is (= 71503 (part-2 sample)))
  (test/is (= 35865985 (part-2 input))))
