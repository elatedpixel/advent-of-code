(ns advent-2015.day02
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "2015/day02"))))

(defn parse-dimensions [line]
  (sequence (map #(Integer/parseInt %))
            (rest (re-matches #"(?<length>\d+)x(?<width>\d+)x(?<height>\d+)" line))))

(defn wrapping-paper [[length width height]]
  (let [areas (sort [(* length width) (* width height) (* height length)])]
    (+ (first areas)
       (* 2 (reduce + areas)))))

(defn ribbon [dimensions]
  (let [[a b _] (sort dimensions)]
    (+ a a b b (reduce * 1 dimensions))))

(defn part1 []
  (transduce (comp (map parse-dimensions)
                   (map wrapping-paper))
             +
             0
             input))

(defn part2 []
  (transduce (comp (map parse-dimensions)
                   (map ribbon))
             +
             0
             input))

(time (println "day02 part1:" (part1)))
(time (println "day02 part2:" (part2)))
