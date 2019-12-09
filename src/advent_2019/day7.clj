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

(defn start [& inputs]
  (computer {:halt?   false
             :input   inputs
             :output  '()
             :pointer 0
             :program input}))
