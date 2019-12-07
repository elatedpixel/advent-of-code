(ns advent-2019.day6
  (:require [advent-2019.core :refer [read-lines]]))

;; https://gist.github.com/stathissideris/1397681b9c63f09c6992
(defn tree-seq-depth
  [branch? children root]
  (letfn [(walk [depth node]
            (lazy-seq
             (cons [node depth]
                   (when (branch? node)
                     (mapcat (partial walk (inc depth)) (children node))))))]
    (walk 0 root)))

(defn orbits [tree]
  (transduce (map second) + (tree-seq-depth next rest tree)))

(defn make-map [input]
  (reduce (fn [m [k v]] (update m k conj v)) {}
          (map #(map keyword (clojure.string/split % #"\)")) input)))

(defn make-tree [m root]
  (lazy-seq
   (cons root
         (map (partial make-tree m) (m root)))))

(defn part1 []
  (let [input   (read-lines "2019/day6.txt")]
    (orbits (make-tree (make-map input) :COM))))

(let [input '("COM)B" "B)C" "C)D" "D)E" "E)F"
                      "B)G" "G)H" "D)I" "E)J" "J)K" "K)L")]
  (orbits (make-tree (make-map input) :COM)))

