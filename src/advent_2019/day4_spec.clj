(ns advent-2019.day4-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]))

;; Your puzzle input is 248345-746315.
(def input (range 248345 (inc 746315)))

(defn six-digit-number? [n]
  (= 6 (count (str n))))

(defn increasing-order? [n]
  (apply <= (map int (str n))))

(defn contains-contiguous-duplicate? [n]
  (some (fn [[a b]] (= a b))
        (partition 2 1 (str n))))

(defn contains-contiguous-unique-duplicate? [n]
  (let [counts (frequencies (str n))]
    (some (fn [[a b]] (and (= a b) (= 2 (counts a))))
          (partition 2 1 (str n)))))

(s/def ::part1 (s/and six-digit-number?
                      increasing-order?
                      contains-contiguous-duplicate?))

(s/def ::part2 (s/and six-digit-number?
                      increasing-order?
                      contains-contiguous-unique-duplicate?))

(defn part1 []
  (count (filter #(s/valid? ::part1 %) input)))

(defn part2 []
  (count (filter #(s/valid? ::part2 %) input)))
