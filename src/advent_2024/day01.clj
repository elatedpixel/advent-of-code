(ns advent-2024.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- delta-abs [a b]
  (if (< a b) (- b a) (- a b)))

(defn -main []
  (let [input     (slurp (io/resource "2024/day01.txt"))
        distances (reduce
                   (fn [distances lists-string]
                     (let [[a b] (str/split lists-string #"\s+")]
                       (-> distances
                           (update :left conj (parse-long a))
                           (update :right conj (parse-long b)))))
                   {}
                   (str/split-lines input))]
    (println
     (reduce #'+ (map delta-abs
                      (sort (:left distances))
                      (sort (:right distances)))))
    (println
     (let [occurrences (frequencies (:right distances))]
       ;; for each element `n` in the left list, multiply `n` by it's occurrences in the right list, and sum
       (transduce (map (fn [n] (* n (get occurrences n 0)))) #'+ 0 (:left distances))))))
