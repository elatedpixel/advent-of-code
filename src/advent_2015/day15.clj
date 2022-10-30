(ns advent-2015.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [s]
  (let [[name props-string] (str/split s #": ")
        properties (into {} (map (fn [[k v]] [(keyword k) v])) (partition 2 (read-string (str "(" props-string ")"))))]
    [(keyword name) properties]))

(def input (into {} (map parse)
                 (line-seq (io/reader (io/resource "2015/day15")))))

(defn- map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn score [ingredients distribution]
  (apply
   merge-with
   +
   (reduce
    (fn [m [ingredient qty]] (conj m (map-vals ingredient (partial * qty))))
    []
    (map vector (vals ingredients) distribution))))

(defn total [recipe]
  (reduce * 1 (vals (map-vals (dissoc recipe :calories) (partial max 0)))))

(def distributions (for [a (range 100)
                         b (range 100)
                         c (range 100)]
                     [a b c (- 100 a b c)]))

(defn part1 []
  (apply max
         (pmap (comp total (partial score input))
               distributions)))

(defn part2 []
  (reduce (fn [a x] (if (= 500 (:calories x))
                      (max a (total x))
                      a))
          0
          (pmap (partial score input) distributions)))

(comment
  (part1)
  ;; => 21367368

  (part2)
  ;; => 1766400

  ;
  )
