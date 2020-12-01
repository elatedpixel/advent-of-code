(ns advent-2020.day01
  (:require [clojure.java.io :as io]))

(def input
  (into #{}
        (map (fn [s] (Integer/parseInt s)))
        (line-seq (io/reader (io/resource "2020/day01")))))

(comment
  (doseq [n input]
    (when-let [pair (input (- 2020 n))]
      (println n "x" pair "=" (* n pair))))
                                        ;part 1
  )

(comment
  (doseq [a input]
    (doseq [b (remove #{a} input)]
      (when-let [c ((set (remove #{a b} input)) (- 2020 a b))]
        (println a "x" b "x" c "=" (* a b c)))))
                                        ;part 2
  )
