(ns advent-2021.day08
  (:require [advent.core :refer (load-input string->sexpression)]
            [clojure.string :as s]
            [clojure.math.combinatorics :refer (permutations)]))

(defn parse-entry [s]
  (map (comp #(map name %) string->sexpression) (s/split s #"\|")))

(def input (map parse-entry (load-input 2021 8)))

(def segment-map
  {0 "abcefg"
   1 "cf"
   2 "acdeg"
   3 "acdfg"
   4 "bcdf"
   5 "abdfg"
   6 "abdefg"
   7 "acf"
   8 "abcdefg"
   9 "abcdfg"})

;; In part 1, we are only counting candidates for digits 1, 4, 7 or 8 in the output values (second item in entry tuple)

(defn silver [output]
  ((into #{} (map (comp count segment-map) [1 4 7 8])) (count output)))

(count
 (sequence
  (comp
   (map second)
   (mapcat #(filter silver %)))
  input))
;; => 381

(def all-permutations (rest (permutations "abcdefg")))
(def valid-segments (set (map set (vals segment-map))))
(def translations (map #(into {} (mapv vector "abcdefg" %)) all-permutations))
(def segments (into {} (map (fn [[k v]] [(set v) k]) segment-map)))

(defn valid-translation? [samples t]
  (when
   (every? #(boolean (valid-segments (set (replace t %)))) samples) t))

(defn find-translation [samples]
  (some (partial valid-translation? samples) translations))

(defn translate-output [t output]
  (Integer/parseInt
    (transduce (map (comp segments set (partial replace t)))
               str
               output)))

(defn translate [[samples output]]
  (translate-output (find-translation samples) output))

(time
 (transduce
  (map translate)
  +
  0
  input))
;; => 1023686
;; "Elapsed time: 1111.49079 msecs"
