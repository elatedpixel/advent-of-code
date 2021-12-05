(ns advent-2021.day05
  (:require [advent.core :refer (load-input string->sexpression)]))

(def input (load-input 2021 5))

(defn expand [a b]
  (cond
    (= a b) (repeat a)
    (< b a) (range a (dec b) -1)
    :else   (range a (inc b))))

(defn overlapping-lines [lines coord->line]
  (->> lines
       (mapcat (comp coord->line string->sexpression))
       frequencies
       vals
       (filter #(< 1 %))
       count))

(defn silver-lines [[x1 y1 _ x2 y2]]
  (when (or (= x1 x2)
            (= y1 y2))
    (map vector (expand x1 x2) (expand y1 y2))))

(defn gold-lines [[x1 y1 _ x2 y2]]
  (map vector (expand x1 x2) (expand y1 y2)))

(overlapping-lines input silver-lines)
;; => 7438

(overlapping-lines input gold-lines)
;; => 21406
