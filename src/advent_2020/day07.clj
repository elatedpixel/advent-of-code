(ns advent-2020.day07
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def input
  (line-seq (io/reader (io/resource "2020/day07"))))

(defn parse-rule [s]
  (re-seq #"((\d+)?\s?(\w+\s\w+) bags?)" s))

;; part 1
(comment

  (def inner->outer
    (reduce-kv
     (fn [m outer inners]
       (reduce (fn [m' inner] (update m' inner conj outer)) m inners))
     {}
     (into {} (map #((juxt first rest) (map last (parse-rule %)))) input)))

  (defn all-bags [color]
    (when (inner->outer color)
      (lazy-cat (inner->outer color)
                (mapcat all-bags (inner->outer color)))))

  (count (set (all-bags "shiny gold"))))

;; part 2
(comment

  (defn count-bags [[number-string color]]
    [(if (= nil number-string) 0 (Integer/parseInt number-string)) color])

  (def outer->inner
    (into {} (comp
              (map parse-rule)
              (map #(map (partial drop 2) %))
              (map (juxt (comp second first) #(map count-bags (rest %))))) input))

  (defn all-counts [color]
    (for [[n bag] (outer->inner color)
          :when (pos? n)]
      (+ n (* n (reduce + 0 (all-counts bag))))))

  (reduce + (all-counts "shiny gold")))
