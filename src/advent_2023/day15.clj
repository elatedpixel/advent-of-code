(ns advent-2023.day15 
  (:require
   [advent.core :as c]
   [clojure.string :as str]))

(def input (first (c/read-lines (c/file 2023 15))))

(defn- hash-lol [result c]
  (rem (* 17 (+ result (int c))) 256))

(defn- part-1 [input]
  (transduce
   (map (partial reduce (completing hash-lol) 0))
   +
   0
   (str/split input #",")))

(part-1 input)
;; => 517315
