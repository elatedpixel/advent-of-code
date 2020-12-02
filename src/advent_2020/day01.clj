(ns advent-2020.day01
  (:require [clojure.java.io :as io]))

(def input
  (into #{}
        (map (fn [s] (Integer/parseInt s)))
        (line-seq (io/reader (io/resource "2020/day01")))))

(comment
  (time
   (take 1
         (for [a     input
               :let  [b (- 2020 a)]
               :when (and (not= a b) (boolean (input b)))]
           (* a b))))

  "Elapsed time: 0.360913 msecs"
  ;; => (970816)
                                        ;part 1
  )

(comment
  (time
   (take 1
         (for [a     input
               b     input
               :let  [c (- 2020 a b)]
               :when (and (not= a b c) (boolean (input c)))]
           (* a b c))))

  "Elapsed time: 0.248882 msecs"
  ;; => (96047280)
                                        ;part 2
  )
