(ns advent-2019.day1
  (:require [clojure.java.io :as io]))

(def input
  (->> "2019/day1.txt"
       io/resource
       io/reader
       line-seq
       (map #(Integer/parseInt %))))

(defn fuel [mass]
  (- (quot mass 3) 2))

(defn part1 [masses]
  (transduce (map fuel) + masses))

(defn total-fuel [mass]
  (->> mass
       (iterate fuel)
       (take-while pos?)
       rest
       (reduce +)))

(defn part2 [masses]
  (transduce (map total-fuel) + masses))

(defn -main []
  (time (println (part1 input)))
  (time (println (part2 input))))
