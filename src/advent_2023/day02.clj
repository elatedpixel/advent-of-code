(ns advent-2023.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day02"))))

(def sample01 "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn- parse [record game]
  (let [[game-str subset-strs] (str/split game #": ")
        id (read-string (re-find #"\d+" game-str))
        subsets (->> (str/split subset-strs #"; ")
                     (map #(str/split % #", "))
                     (map #(into {} (map (fn [s] (let [[dice color] (str/split s #" ")]
                                                   {color (read-string dice)})))
                                 %)))]
    (assoc record id subsets)))

(defn- possible? [rules [_ subsets]]
  (not-any? false?
            (for [subset subsets]
              (every? (fn [[color dice]] (and (contains? rules color)
                                              (<= dice (rules color))))
                      subset))))

(defn part-1 [input]
  (let [rules          {"red"   12
                        "green" 13
                        "blue"  14}
        games          (reduce parse {} (str/split-lines input))
        possible-games (filter (partial possible? rules) games)]
    (transduce (map first)              ; collect ids
               +                        ; sum them
               possible-games)))

(test/is (= 8 (part-1 sample01)))
(test/is (= 2679 (part-1 input)))

(defn power [[_id subsets]]
  (reduce * 1 (vals (apply merge-with max subsets))))

(defn part-2 [input]
  (let [games (reduce parse {} (str/split-lines input))]
    (transduce (map power) + games)))

(test/is (= 2286 (part-2 sample01)))
(test/is (= 77607 (part-2 input)))

