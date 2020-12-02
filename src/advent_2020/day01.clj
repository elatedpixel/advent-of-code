(ns advent-2020.day01
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input
  (into #{}
        (map (fn [s] (Integer/parseInt s)))
        (line-seq (io/reader (io/resource "2020/day01")))))

(comment
  (time
   (println
    (take 1
          (for [a     (sort input)
                :let  [b (- 2020 a)]
                :when (and (not= a b) (boolean (input b)))]
            (* a b)))))

  "Elapsed time: 1.179171 msecs"
  ;; => (970816)
                                        ;part 1
  )

(comment
  (time
   (println
    (let [sorted-input (sort input)]
      (take 1
            (for [a     sorted-input
                  b     sorted-input
                  :let  [c (- 2020 a b)]
                  :when (and (not= a b c) (boolean (input c)))]
              (* a b c))))))

  "Elapsed time: 1.219552 msecs"
  ;; => (96047280)
                                        ;part 2
  )
