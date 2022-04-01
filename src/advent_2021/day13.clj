(ns advent-2021.day13
  (:require [advent.core :as c]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2021/day13")))

(defn parse-instruction [s]
  (let [[axis value] (rest (re-find #"(x|y)=(\d+)" s))]
    [axis (Integer/parseInt value)]))

(let [[coords instructions] (s/split input #"\n\n")]
  (def coords (set (mapv c/string->sexpression (s/split-lines coords))))
  (def instructions (map parse-instruction (s/split-lines instructions))))

(def rows (inc (apply max (map second coords))))
(def cols (inc (apply max (map first coords))))

(defn make-grid [rows columns default]
  (vec (repeat rows (vec (repeat columns default)))))

(def grid
  (reduce (fn [m [x y]] (assoc-in m [y x] \#))
          (make-grid rows cols \.)
          coords))

(defn merge-dot [a b] (if (= \# a) a b))

(defn fold [grid [axis value]]
  (case axis
    "y" (mapv (partial mapv merge-dot) (subvec grid 0 value) (rseq (subvec grid (inc value))))
    "x" (mapv (fn [row] (mapv merge-dot (subvec row 0 value) (rseq (subvec row (inc value))))) grid)))

((frequencies (flatten (fold grid (first instructions)))) \#)
;; => 689

(println (s/join \newline (map (partial apply str) (reduce fold grid instructions))))
;; ###..#....###...##....##..##..#....#..#.
;; #..#.#....#..#.#..#....#.#..#.#....#..#.
;; #..#.#....###..#.......#.#....#....#..#.
;; ###..#....#..#.#.......#.#.##.#....#..#.
;; #.#..#....#..#.#..#.#..#.#..#.#....#..#.
;; #..#.####.###...##...##...###.####..##..
