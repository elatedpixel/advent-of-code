(ns advent-2019.day1
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "2019/day1.txt")
       (io/reader)
       (line-seq)
       (map #(Integer/parseInt %))))

(defn part1 [module]
  (- (int (/ module 3)) 2))

(defn part2 [module]
  (->> module
       (iterate part1)
       (take-while pos?)
       (drop 1)
       (reduce +)))

(defn -main []
  (time (println (reduce + (map part1 input))))
  (time (println (reduce + (map part2 input)))))

(comment
  (part2 100756)
  ;; => 50346
  )
