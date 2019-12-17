(ns advent-2019.day7
  (:require
   [clojure.test :as t]
   [advent-2019.core :refer [read-lines]]
   [advent-2019.intcode :refer (computer)]))

(def input
  (->> (read-lines "2019/day7.txt")
       first
       (format "(%s)")
       read-string
       vec))

(def phase-settings
  (for [a (range 5)
        b (range 5)
        c (range 5)
        d (range 5)
        e (range 5)
        :when (= 5 (count (set (list a b c d e))))]
    (list a b c d e)))

(defn start [& inputs]
  (computer {:halt?   false
             :input   inputs
             :output  '()
             :pointer 0
             :program input}))
