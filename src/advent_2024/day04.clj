(ns advent-2024.day04
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def ^:private puzzle (line-seq (io/reader (io/resource "2024/day04.txt"))))
(def ^:private sample0 (line-seq (io/reader (io/resource "2024/day04.sample0"))))

(defn- word-search
  "Returns a word-search grid (map[(x,y)]char) from list-of-strings."
  [list-of-strings]
  (reduce
   (fn [m [y s]]
     (reduce
      (fn [m [x c]]
        (assoc m [x y] c))
      m
      (map-indexed vector s)))
   {}
   (map-indexed vector list-of-strings)))

(let [offsets-list
      [[[0 0] [0 -1] [0 -2] [0 -3]]
       [[0 0] [1 -1] [2 -2] [3 -3]]
       [[0 0] [1 0] [2 0] [3 0]]
       [[0 0] [1 1] [2 2] [3 3]]
       [[0 0] [0 1] [0 2] [0 3]]
       [[0 0] [-1 1] [-2 2] [-3 3]]
       [[0 0] [-1 0] [-2 0] [-3 0]]
       [[0 0] [-1 -1] [-2 -2] [-3 -3]]]]
  (defn- neighbors [grid]
    (fn [[x y]]
      (keep (fn [offsets]
              (let [coords (mapv (fn [[x' y']] [(+ x x') (+ y y')]) offsets)]
                (when (every? #(grid %) coords)
                  (apply str (map grid coords)))))
            offsets-list))))


;; count occurrences of "XMAS" in word-search grid
(defn part1 [input]
  (let [grid (word-search input)]
    ((frequencies (mapcat (neighbors grid) (keys grid))) "XMAS")))

(t/deftest test-part1
  (t/is (= 18 (part1 sample0))))

(defn part2 [input] 0)

(t/deftest test-part2
  (t/is (= 0 (part2 sample0))))

(defn -main []
  (println (str "Day 4 Part 1: " (part1 puzzle))))
