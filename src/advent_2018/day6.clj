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

  (defn manhattan [[x1 y1] [x2 y2]]
    (+ (Math/abs (- x1 x2))
       (Math/abs (- y1 y2))))

  (is (= 7 (manhattan [4 0] [0 3]))))

(with-test

  (defn get-bounds [points]
    (let [min-max (juxt min max)
          [xs ys] (apply mapv vector points)]
      [(apply min-max xs) (apply min-max ys)]))

  (is (= [[2 9] [2 10]]
         (get-bounds [[5 4] [3 3] [9 2] [2 10]]))))

(defn rectangle-select [[[ca cb] [ra rb]] freq]
  (into {}
        (filter (fn [[[c r] _]] (and (< ca c cb)
                                     (< ra r rb))))
        freq))

(with-test

  (defn owners [[[min-x max-x] [min-y max-y]] points]
    (reduce
     (fn [m {:keys [x y] :as coord}]
       (cond (or (= x min-x) (= x max-x)
                 (= y min-y) (= y max-y))
             (assoc m coord nil)

             :else
             (let [sorted-points (sort-by (partial manhattan coord) points)]
               (assoc m coord (if (= (nth sorted-points 0)
                                     (nth sorted-points 1 nil))
                                nil
                                (first sorted-points))))))

     {}
     (map vector
          (range min-x (inc max-x))
          (range min-y (inc max-y)))))

  (is (= {[0 0] [1 1]
          [1 0] [1 1]
          [0 1] [1 1]
          [1 1] [1 1]}
         (owners [[0 2] [0 2]] sample))))

(run-tests 'advent-2018.day6)

(defn solve-part-1 [data]
  (let [bounds (get-bounds data)]
    #_(apply max-key val (rectangle-select
                          bounds
                          (frequencies (vals (knn bounds data)))))
    (->> data
         (owners bounds)
         vals
         frequencies
         (#(dissoc % nil))
         (apply max-key val))))

(comment
  (let [data data]
    (solve-part-1 data))
 ;
  )

(defn -main [& args]
  (prn)
  (println (str "Day 6 Part 1: " (time (solve-part-1 data)))))
