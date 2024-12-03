(ns advent-2024.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defonce input (line-seq (io/reader (io/resource "2024/day01.txt"))))

(defn- delta-abs [a b]
  (if (< a b) (- b a) (- a b)))

(defn- parse-lists [lists-strings]
  (->> lists-strings
       (map (fn [s] (read-string (format "(%s)" s))))
       (apply mapv (comp sort vector))))

(defn -main []
  (let [lists (parse-lists input)]
    ;; part 1, sum delta between two sorted lists
    (println
     (reduce #'+ (apply map delta-abs lists)))

    ;; part 2, for each element `n` in the left list, multiply `n` by it's occurrences in the right list, and sum
    (println
     (let [occurrences (frequencies (second lists))]
       (transduce (map (fn [n] (* n (get occurrences n 0)))) #'+ 0 (first lists))))))
