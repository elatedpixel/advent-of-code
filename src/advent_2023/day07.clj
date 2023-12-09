(ns advent-2023.day07 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day07"))))
(def sample (str/trim (slurp (io/resource "2023/day07.sample"))))

(def ^:private hands {[1 1 1 1 1] 0, [1 1 1 2] 1, [1 2 2] 2, [1 1 3] 3, [2 3] 4, [1 4] 5, [5] 6})

(defn- camel-hand [s]
  (let [[hand bid] (str/split s #"\s")]
    {:hand (str/escape hand (zipmap "TJQKA" "ABCDE"))
     :bid  (parse-long bid)
     :rank (hands (vec (sort (vals (frequencies hand)))))}))

(defn- camel-cards
  [input]
  (->> input
       (str/split-lines)
       (map camel-hand)))

(defn- total-winnings [camel-cards]
  (transduce
   (map-indexed (fn [rank card] (* (inc rank) (:bid card))))
   +
   (sort-by (juxt :rank :hand) camel-cards)))

(defn- part-1
  [input]
  (total-winnings (camel-cards input)))

(test/deftest test-part-1
  (test/is (= 6440 (part-1 sample)))
  (test/is (= 251121738 (part-1 input))))
