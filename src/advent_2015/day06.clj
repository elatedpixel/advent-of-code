(ns advent-2015.day06
  (:require [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "2015/day06"))))

(def re-instruction
  #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

(defn parse-instruction
  "parse input text commands and return sequence of operations to perform"
  [text]
  (let [[instruction from-y from-x to-y to-x]
        (rest (re-matches re-instruction text))]
    (for [y (range (Integer/parseInt from-y) (inc (Integer/parseInt to-y)))
          x (range (Integer/parseInt from-x) (inc (Integer/parseInt to-x)))]
      #(update %1 [y x] (%2 instruction)))))

(defn- run [fn]
  (reduce +' (vals (transduce (mapcat parse-instruction)
                              (completing (fn [grid f] (f fn grid)))
                              {}
                              input))))

(defn update-1 [instruction]
  (case instruction
    "toggle"   (fnil #(bit-xor % 1) 0)
    "turn on"  (constantly 1)
    "turn off" (constantly 0)))

(comment
  (run update-1)
  ;; 569999
  )

(defn update-2 [instruction]
  (case instruction
    "toggle"   (fnil (comp inc inc) 0)
    "turn on"  (fnil inc 0)
    "turn off" (fnil (comp #(max 0 %) dec) 0)))

(comment
  (run update-2)
;; => 17836115
  )
