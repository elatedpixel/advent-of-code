(ns advent-2016.day3
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def data
  (->> "2016/day3.txt"
       io/resource
       io/reader
       line-seq
       (map (comp read-string #(str "[" % "]")))))

(t/with-test

  (defn triangle? [[a b c]]
    (and
     (< a (+ b c))
     (< b (+ a c))
     (< c (+ a b))))

  (t/is (= false (triangle? [5 10 15])))
  (t/is (= true (triangle? [5 12 15]))))

(defn part1 [xs]
  (filter triangle? xs))

(defn -main []
  (->> data
       part1
       count
       println
       time)
  (->> data
       (apply mapcat vector)
       (partition 3)
       part1
       count
       println
       time))

(t/run-tests 'advent-2016.day3)
