(ns advent-2024.day13
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn- parse-numbers [s]
  (into [] (map parse-long) (re-seq #"\d+" s)))

(defrecord ClawMachine [a b prize])

(defn- parse [input]
  (mapv
   (fn [line]
     (let [[button-a button-b prize] (s/split-lines line)]
       (ClawMachine.
         (parse-numbers button-a)
         (parse-numbers button-b)
         (parse-numbers prize))))
   (s/split input #"\n\n")))

(def ^:private sample0 (parse (slurp (io/resource "2024/day13.sample0"))))
(def ^:private puzzle (parse (slurp (io/resource "2024/day13.txt"))))

(defn- tokens
  [^ClawMachine {[ax ay] :a
                 [bx by] :b
                 [x y]   :prize}]
  (let [a (/ (- x (* y (/ bx by)))
             (- ax (* ay (/ bx by))))
        b (/ (- y (* a ay)) by)]
    (if (every? integer? [a b])
      (+ (* 3 a) b)
      0)))

(defn part-1 [puzzle]
  (transduce (map tokens) + puzzle))

(defn- vec+ [v c] (mapv (partial + c) v))

(defn part-2 [puzzle]
  (transduce (comp
               (map (fn [machine] (update machine :prize vec+ 10000000000000)))
               (map tokens)) + puzzle))

(t/deftest test-part-1
  (t/is (= 480 (part-1 sample0))))

(defn -main []
  (time (println "Day 13 Part 1: " (part-1 puzzle)))
  (time (println "Day 13 Part 2: " (part-2 puzzle))))
