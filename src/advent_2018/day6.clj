(ns advent-2018.day6
  (:require [clojure.test :refer [with-test is run-tests]]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]))

(def sample '((1 1)
              (1 6)
              (8 3)
              (3 4)
              (5 5)
              (8 9)))

(def data (->> (io/resource "2018/day6.txt")
               (io/reader)
               (line-seq)
               (map #(read-string (format "(%s)" %)))))

(with-test
  (defn manhattan-distance [[^long x1 ^long y1] [^long x2 ^long y2]]
    (+ (Math/abs (- x1 x2))
       (Math/abs (- y1 y2))))
  (is (= 7 (manhattan-distance [4 0] [0 3]))))

(with-test
  (defn get-bounds [points]
    (let [min-max (juxt min max)
          [xs ys] (apply mapv vector points)]
      [(apply min-max xs) (apply min-max ys)]))
  (is (= [[2 9] [2 10]]
         (get-bounds [[5 4] [3 3] [9 2] [2 10]]))))

(print (get-bounds data))

(defn rectangle-select [[[ca cb] [ra rb]] freq]
  (into {}
        (filter (fn [[[c r] _]] (and (< ca c cb)
                                     (< ra r rb))))
        freq))

(with-test
  (defn knn [[cols rows] points]
    (loop [m {}
           [coord & coords] (for [r (apply range rows)
                                  c (apply range cols)]
                              [c r])]
      (cond (nil? coord) m
            :else (recur
                   (let [sorted-points (sort-by (partial manhattan-distance coord) points)]
                     (if (= (nth sorted-points 0)
                            (nth sorted-points 1 nil))
                       m                ; there is a manhattan distance tie
                       (assoc m coord (first sorted-points))))
                   coords))))
  (is (= {[0 0] [1 1]
          [1 0] [1 1]
          [0 1] [1 1]
          [1 1] [1 1]}
         (knn [[0 2] [0 2]] [[1 1]]))))

(run-tests 'advent-2018.day6)

(defn print-bounds [bounds])

(defn solve-part-1 [data]
  (let [bounds (get-bounds data)]
    (apply max-key val (rectangle-select
                       bounds
                       (frequencies (vals (knn bounds data)))))))

(comment
  (let [data sample]
    (solve-part-1 data))
  ;
  )

(defn -main [& args]
  (prn)
  (println (str "Day 6 Part 1: " (time (solve-part-1 data)))))
