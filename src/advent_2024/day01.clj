(ns advent-2024.day01
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn- distance [a b]
  (if (< a b)
    (- b a)
    (- a b)))

(defn -main []
  (let [input
        (slurp (io/resource "2024/day01.txt"))
        distances
        (reduce
         (fn [distances lists-string]
           (let [[a b] (str/split lists-string #"\s+")]
             (-> distances
                 (update :left conj (parse-long a))
                 (update :right conj (parse-long b)))))
         {}
         (str/split-lines input))]
    (pprint/pprint
     (reduce #'+
             (map distance
                  (sort (:left distances))
                  (sort (:right distances)))))
    (pprint/pprint
     (let [m (frequencies (:right distances))]
       (reduce #'+
               0
               (map (fn [n] (* n (get m n 0)))
                    (:left distances)))))))
