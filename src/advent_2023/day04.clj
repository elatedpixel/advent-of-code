(ns advent-2023.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]
   [clojure.walk :as walk]
   [clojure.set :as set]))

(def input (str/trim (slurp (io/resource "2023/day04"))))
(def sample (str/trim (slurp (io/resource "2023/day04.sample"))))

(defn- points [game]
  (let [n (dec (count game))]
    (if (neg? n) 0
        (reduce * 1 (repeat n 2)))))

(defn- parse
  [input]
  (->> input
       (str/trim)
       (str/split-lines)
       (map (comp #(str/split % #"\|")
                  second
                  #(str/split % #": ")))
       (walk/postwalk #(if (string? %) (into #{} (map read-string) (re-seq #"\d+" %)) %))
       (map (partial apply set/intersection))))

(defn part-1 [input]
  (transduce (map points) + (parse input)))

(test/is (= 13 (part-1 sample)))
(test/is (= 15268 (part-1 input)))

(defn- scratchcards
  [input]
  (let [cards      (parse input)
        scratchers (into {} (map-indexed (fn [i _card] (vector (inc i) 1)) cards))
        matches    (into {} (map-indexed (fn [i card] (vector (inc i) (count card))) cards))]
    (loop [i          1
           scratchers scratchers]
      (if (nil? (scratchers i))
        scratchers
        (recur (inc i)
               (let [v (scratchers i)]
                 (if (pos? v) (reduce (fn [m j] (update m j  (fnil + 0) (m i)))
                                      scratchers
                                      (range (inc i) (+ 1 i (matches i))))
                     scratchers)))))))

(defn part-2 [input]
  (reduce + (vals (scratchcards input))))

(test/is (= 30 (part-2 sample)))
(test/is (= 6283755 (part-2 input)))
