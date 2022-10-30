(ns advent-2015.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "2015/day16" io/resource io/reader line-seq))
(def regex #"Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)")

(def clue {"samoyeds"    "2",
           "cats"        "7",
           "children"    "3",
           "pomeranians" "3",
           "vizslas"     "0",
           "goldfish"    "5",
           "akitas"      "0",
           "perfumes"    "1",
           "cars"        "2",
           "trees"       "3"})

(defn- parsue [sue]
  (let [[match? id & clues] (first (re-seq regex sue))]
    (list id (into {} (map vec) (partition 2 clues)))))

(defn- sue? [sue]
  (= sue (select-keys clue (keys sue))))

(defn- pursue [pred]
  (fn [m sue]
    (let [[id sue] (parsue sue)]
      (if (pred sue) (assoc m id sue) m))))

(def part1
  (delay (reduce (pursue sue?) {} input)))

(defn- retroencabulator [sue]
  (reduce
   (fn [b [k v]]
     (and b
          (case k
            ("cats" "trees") (> (Integer/parseInt v) (Integer/parseInt (get clue k)))
            ("pomeranians" "goldfish") (< (Integer/parseInt v) (Integer/parseInt (get clue k)))
            (= v (get clue k)))))
   true
   sue))

(def part2
  (delay (reduce (pursue retroencabulator) {} input)))

(comment
  @part1
  ;; => {"40" {"vizslas" "0", "cats" "7", "akitas" "0"}}

  @part2
  ;; => {"241" {"cars" "2", "pomeranians" "1", "samoyeds" "2"}}

  )
