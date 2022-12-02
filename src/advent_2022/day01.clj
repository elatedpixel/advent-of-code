(ns advent-2022.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def parser (comp
             (map str/split-lines)
             (map (partial map read-string))
             (map (partial reduce +))))

(def input
  (->> (str/split (slurp "resources/2022/day01") #"\n\n")
       (sequence parser)
       (sort >)))

(defn n-sum
  ([n] (n-sum n input))
  ([n coll]
   (transduce (take n) + coll)))

(comment

  ;; part 1
  (n-sum 1)
  ;; => 70720

  ;; part 2
  (n-sum 3)
  ;; => 207148
  )
