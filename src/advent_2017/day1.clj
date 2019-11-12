(ns advent-2017.day1
  (:require [clojure.java.io :as io]))

(defn captcha1
  [input]
  (->> input
       (cycle)
       (partition 2 1)
       (take (count input))
       (reduce (fn [r [a b]] (if (= a b) (+ r (Integer/parseInt (str a))) r)) 0)))

(defn -main []
  (let [input (slurp (io/resource "2017/day1.txt"))]
    (prn)
    (println (time (captcha1 input)))))
