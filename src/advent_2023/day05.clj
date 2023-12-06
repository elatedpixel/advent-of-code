(ns advent-2023.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]
   [criterium.core :as criterium]))

(def input (str/trim (slurp (io/resource "2023/day05"))))
(def sample (str/trim (slurp (io/resource "2023/day05.sample"))))

(defn- to-ints
  [s]
  (mapv read-string (re-seq #"\d+" s)))

(defn- range*
  "we need to use this like a pair in a `cond` statement, returns (pred fn)"
  [[dst src len]]
  (list
   (fn [n] (<= src n (dec (+ len src))))
   (fn [n] (+ n (- dst src)))))

(defn- lookup [r]
  (fn [x] (reduce
           (fn [x [pred f]] (if (pred x) (reduced (f x)) x))
           x
           (partition 2 r))))

(defn- resolve* [m]
  (comp
   (lookup (:humidity-to-location m))
   (lookup (:temperature-to-humidity m))
   (lookup (:light-to-temperature m))
   (lookup (:water-to-light m))
   (lookup (:fertilizer-to-water m))
   (lookup (:soil-to-fertilizer m))
   (lookup (:seed-to-soil m))))
;; Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.

(defn- parse [input]
  (let [sections (str/split input #"\n\n")]
    (reduce
     (fn [m s]
       (cond
         (.startsWith s "seeds")
         (assoc m :seeds (to-ints s))

         :else
         (let [[name-str & ranges-str] (str/split-lines s)
               name                    (keyword (re-find #"[\w-]+" name-str))
               ranges                  (mapcat (comp range* to-ints) ranges-str)]
           (assoc m name ranges))))
     {}
     sections)))

(defn part-1 [input]
  (let [almanac (parse input)]
    (apply min (map (resolve* almanac) (:seeds almanac)))))

(criterium/quick-bench (part-1 input))
;; => 600279879
;; Evaluation count : 120 in 6 samples of 20 calls.
;;              Execution time mean : 5.565616 ms
;;     Execution time std-deviation : 227.994195 µs
;;    Execution time lower quantile : 5.317659 ms ( 2.5%)
;;    Execution time upper quantile : 5.791945 ms (97.5%)
;;                    Overhead used : 16.002083 ns

(test/deftest part1
  (test/is (= 35 (part-1 sample))))
