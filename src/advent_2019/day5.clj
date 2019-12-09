(ns advent-2019.day5
  (:require [advent-2019.core :refer [read-lines]]
            [advent-2019.intcode :refer [computer]]
            [clojure.test :as t]))

(def input
  (->> (read-lines "2019/day5.txt")
       first
       (format "(%s)")
       read-string
       vec))

(defn start [i]
  (computer {:halt?   false
             :input   i
             :output  0
             :pointer 0
             :program input}))
