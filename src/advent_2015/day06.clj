(ns advent-2015.day06
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "2015/day06"))))

(def re-instruction
  #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

(defn perform [instruction]
  (case instruction
    "toggle"   (fnil #(bit-xor % 1) 0)
    "turn on"  (constantly 1)
    "turn off" (constantly 0)))

(defn parse-instruction [text]
  (let [[instruction from-y from-x to-y to-x]
        (rest (re-matches re-instruction text))]
    (for [y (range (Integer/parseInt from-y) (inc (Integer/parseInt to-y)))
          x (range (Integer/parseInt from-x) (inc (Integer/parseInt to-x)))]
      #(update % [y x] (perform instruction)))))

(comment
  (reduce
   +'
   (vals
    (transduce
     (mapcat parse-instruction)
     (completing (fn [grid f] (f grid)))
     {}
     input)))
  ;; 569999
  )



