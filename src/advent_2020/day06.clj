(ns advent-2020.day06
  (:require [clojure.string :as s]
            [clojure.set :as st]
            [clojure.java.io :as io]))

(def input
  (s/split (slurp (io/resource "2020/day06")) #"\n\n"))

;; part 1
(time (transduce (comp
                  (map #(re-seq #"[^\n]" %))
                  (map set)
                  (map count))
                 +
                 0
                 input))

;; part 2
(time (transduce (comp
                  (map s/split-lines)
                  (map #(map set %))
                  (map #(apply st/intersection %))
                  (map count))
                 +
                 0
                 input))
