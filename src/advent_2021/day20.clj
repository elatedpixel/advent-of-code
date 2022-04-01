(ns advent-2021.day20
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :refer (pprint)]))

(defn- to-hashmap [coll]
  (into {}
        (for [y (range (count coll))
              x (range (count (get coll y)))]
          [[y x] (get-in coll [y x])])))

(defn- neighbors [coord]
  (for [y (range -1 2)
        x (range -1 2)]
    (mapv + coord [y x])))

(defn- coord->binary [m coord default]
  (Integer/parseInt
   (apply str (map #(get m % default) (neighbors coord)))
   2))

(defn- expand [image]
  (let [minmax (juxt (partial apply min)
                     (comp inc (partial apply max)))
        [y1 y2] (minmax (map first (keys image)))
        [x1 x2] (minmax (map second (keys image)))]
    (for [y (range (dec y1) (inc y2))
          x (range (dec x1) (inc x2))]
      [y x])))

(defn- enhance [algorithm]
  (fn [[image n]]
    (let [default (if (even? n) \1 \0) ; my puzzle input has an algorithm beginning with \# and ending with \. and so the infinite space is blinking
          binary  (memoize #(coord->binary image % default))]
      [(reduce
        (fn [m x] (assoc m x (get algorithm (binary x))))
        {}
        (expand image))
       (inc n)])))

(def file-input (slurp (io/resource "2021/day20")))
(def example-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###")

(let [input        (str/split file-input #"\n\n")
      algorithm    (zipmap (range 0 512) (replace {\# \1 \. \0} (first input)))
      image        (to-hashmap (mapv #(vec (replace {\# \1 \. \0} %))
                                     (str/split-lines (second input))))
      enhancements (map first (iterate (enhance algorithm) [image 1]))]
  (println "Silver:" ((comp frequencies vals) (nth enhancements 2)))
  (println "Gold:" ((comp frequencies vals) (nth enhancements 50))))
