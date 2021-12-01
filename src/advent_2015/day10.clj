(ns advent-2015.day10
  (:require
   [clojure.java.io :as io]))

(def input (slurp (io/resource "2015/day10")))

(defn look-say [s]
  (apply str
         (mapcat (juxt count first)
                 (partition-by identity s))))

(defn nth-look-say [n]
  (count (last (take (inc n) (iterate look-say input)))))

;; part 1
(nth-look-say 40);; => 360154

;; part 2
(nth-look-say 50);; => 5103798
