(ns advent-2024.day11
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def ^:private sample0 "0 1 10 99 999")
(def ^:private sample1 "125 17")
(def ^:private puzzle (slurp (io/resource "2024/day11.txt")))

;; Every time you blink, the stones each simultaneously change according to the first applicable rule in this list:
;; * If the stone is engraved with the number 0, it is replaced by a stone
;;   engraved with the number 1.
;; * If the stone is engraved with a number that has an even number of digits,
;;   it is replaced by two stones. The left half of the digits are engraved on
;;   the new left stone, and the right half of the digits are engraved on the
;;   new right stone. (The new numbers don't keep extra leading zeroes: 1000
;;   would become stones 10 and 0.)
;; * If none of the other rules apply, the stone is replaced by a new stone; the
;;   old stone's number multiplied by 2024 is engraved on the new stone.
(defn- transform
  [stone]
  (let [stone-string (str stone)]
    (cond
      (zero? stone)                (list 1)
      (even? (count stone-string)) (sequence
                                     (map (comp parse-long #(apply str %)))
                                     (split-at (long (/ (count stone-string) 2))
                                               stone-string))
      :else                        (list (* stone 2024)))))

(defn- blink
  ([stones]
   "multiset impl of stones state."
   (reduce
     (fn [m [k v]]
       (reduce (fn [m k']
                 (update m k' (fnil +' 0) v))
               m
               (transform k)))
     {}
     stones))
  ([stones n]
   (loop [i 0 s stones]
     (if (= i n) s (recur (inc i) (blink s))))))

(defn- parse
  [input]
  (frequencies (read-string (format "(%s)" input))))

(defn part-1
  [input]
  (reduce +' (vals (blink (parse input) 25))))

(defn part-2
  [input]
  (reduce +' (vals (blink (parse input) 75))))

(t/deftest test-part-1
  (t/is (= 55312 (part-1 sample1))))

(defn -main []
  (println (time (str "Day 11 Part 1: " (part-1 puzzle))))
  (println (time (str "Day 11 Part 2: " (part-2 puzzle)))))
