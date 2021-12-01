(ns advent-2015.day11
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(def input (line-seq (io/reader (io/resource "2015/day11"))))

(defn str->ints [s] (map int s))

(defn ints->str [ns] (transduce (map char) str ns))

(defn str->number [s]
  (Long/parseUnsignedLong
    (reduce
     str
     (map #(Long/toUnsignedString (- (int %) 97) 26) s))
    26))

(defn number->str [n]
  (reduce str
          (map #(char (+ 97 (Long/parseUnsignedLong (str %) 26)))
               (Long/toUnsignedString n 26))))

(defn increment-string [s]
  (let [a (int \a)
        z (int \z)]
    (ints->str
     ())))

(defn dont-contain? [s] (complement #(re-find (re-pattern (str "[" s "]")) %)))

(defn increasing-straight? [n]
  (fn [s]
    (boolean
      (some (fn [[a b c]] (= a (dec b) (dec (dec c))))
            (partition n 1 (str->ints s))))))

(defn non-overlapping-pairs [[x & xs]]
  (when (seq x)
    (if (apply = x)
      (lazy-seq (cons x (non-overlapping-pairs (rest xs))))
      (lazy-seq (non-overlapping-pairs xs)))))

(defn n-non-overlapping-pairs? [n]
  (fn [s] (<= n (count (take n (non-overlapping-pairs (partition 2 1 s)))))))

(def password-requirements
  (every-pred (increasing-straight? 3)
              (dont-contain? "iol")
              (n-non-overlapping-pairs? 2)))

(defn rotate-password [previous]
  (some #(when (password-requirements %) %)
        (sequence (map number->str) (rest (iterate inc (str->number previous))))))

(comment
  ;; part 1
  (def part1 (rotate-password (first input)))
  ;; => "cqjxxyzz"

  ;; part 2
  (def part2 (rotate-password part1))
  ;; => "cqkaabcc"
  )
