(ns advent-2024.day06
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def ^:private puzzle
  (line-seq (io/reader (io/resource "2024/day06.txt"))))
(def ^:private sample0
  (line-seq (io/reader (io/resource "2024/day06.sample0"))))

(defn- grid
  [lines]
  (into {}
        (for [y (range (count lines))
              x (range (count (first lines)))]
          [[x y] (nth (nth lines y) x)])))

(defn- parse
  [input]
  (let [lab (grid input)]
    {:lab   lab
     :done? false
     :loop? false
     :seen  #{}
     :guard (some (fn [[k v]] (when ((set "<>^v") v) k)) lab)}))

;; The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

;; Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:
;; - If there is something directly in front of you, turn right 90 degrees.
;; - Otherwise, take a step forward.

(defn- step
  "Returns next coordinate, given direction character (<,>,^,v) and [x, y]."
  [direction [x y]]
  (case direction
    \< [(dec x) y]
    \> [(inc x) y]
    \^ [x (dec y)]
    \v [x (inc y)]
    (throw (Exception. (str "Invalid direction provided to (step): " direction)))))

(defn- turn
  "Turns guard right 90°."
  [guard]
  (case guard
    \< \^
    \> \v
    \^ \>
    \v \<
    (throw (Exception. "Invalid guard character provided to (turn)."))))

(defn- walk
  "Implements the simple guard patrol protocol; walk or turn; returns next coordinate or nil."
  [{:keys [lab guard seen] :as m}]
  (let [facing (get lab guard)
        next   (step facing guard)]
    (if (:loop? m)
      (assoc m :done? true)
      (case (get lab next)
        \.  (-> m
                (update :seen conj (conj guard facing))
                (assoc :loop? (some? (seen (conj guard facing))))
                (assoc-in [:lab guard] \.)
                (assoc-in [:lab next] facing)
                (assoc :guard next))
        \#  (update-in m [:lab guard] turn)
        nil (assoc m :done? true)))))

(defn part-1 [input]
  (let [lab (parse input)]
    (count (distinct (map :guard (take-while (complement :done?) (iterate walk lab)))))))

(t/deftest test-part-1
  (t/is (= 41 (part-1 sample0))))

(defn part-2
  [input]
  (let [lab  (parse input)
        path (distinct (eduction (take-while (complement :done?)) (map :guard) (iterate walk lab)))]
    (count
      (filter :loop?
              (map
                (fn [coord] (last (sequence (take-while (complement :done?))
                                            (iterate walk (assoc-in lab [:lab coord] \#)))))
                (rest path))))))

(t/deftest test-part-2
  (t/is (= 6 (part-2 sample0))))

(defn -main []
  (println (str "Day 6 Part 1: " (part-1 puzzle)))
  (println (str "Day 6 Part 2: " (part-2 puzzle))))
