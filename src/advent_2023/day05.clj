(ns advent-2023.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]
   [criterium.core :as criterium]))

(def input (str/trim (slurp (io/resource "2023/day05"))))
(def sample (str/trim (slurp (io/resource "2023/day05.sample"))))

(defn- parse-ints [s] (mapv parse-long (re-seq #"\d+" s)))

(defn- offset [[dst src len]] [src (+ src len -1) (- dst src)])

(defn- parse-rule [[header & ranges]]
  (let [[_ source target] (re-find #"^(\w+)-to-(\w+)" header)]
    [source [target (sort (map (comp offset parse-ints) ranges))]]))

(defn- almanac [input]
  (let [[seed-str & rules-str] (str/split input #"\n\n")]
    {:seeds (parse-ints seed-str)
     :rules (into {} (map (comp parse-rule str/split-lines)) rules-str)}))

(defn- shift [v dx]
  (mapv (partial + dx) v))

(defn split-range-and-shift [[a b :as i] [c d delta]]
  (cond
    (and (< a c) (< d b))           [[a (dec c)] (shift [c d] delta) [(inc d) b]] ;; inside
    (and (< a c) (<= c b) (<= b d)) [[a (dec c)] (shift [c b] delta)] ;; right overlap
    (and (<= c a) (<= a d) (< d b)) [(shift [a d] delta) [(inc d) b]] ;; left overlap
    (and (<= c a) (<= b d))         [(shift i delta)] ;; covering
    :else                           [i])) ;; outside

(defn split-by-rule [rules seed]
  (reduce
   (fn [[[_ b :as curr] & rst] [_ d :as rule]]
     (let [seeds' (into rst (split-range-and-shift curr rule))]
       (if (>= d b)
         (reduced seeds')
         seeds')))
   (list seed)
   rules))

(defn process-rule [rules [name seeds]]
  (if (= "location" name)
    (apply min (flatten seeds))
    (let [[target rule] (rules name)]
      (recur rules [target (mapcat (partial split-by-rule rule) seeds)]))))

(defn find-location [{:keys [seeds rules]} seed-fn]
  (process-rule rules ["seed" (seed-fn seeds)]))

(defn- seeds-silver [seeds]
  (mapv vector seeds seeds))

(defn- seeds-gold [seeds]
  (mapv (fn [[a b]] [a (+ a b -1)])
        (partition 2 seeds)))

(defn part-1 [input]
  (find-location (almanac input) seeds-silver))

(test/deftest part1
  (test/is (= 35 (part-1 sample)))
  (test/is (= 600279879 (part-1 input))))

(defn part-2 [input]
  (find-location (almanac input) seeds-gold))

(test/deftest part2
  (test/is (= 46 (part-2 sample)))
  (test/is (= 20191102 (part-2 input))))

