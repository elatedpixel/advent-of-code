(ns advent.core
  (:require [clojure.data.priority-map :refer [priority-map]]))

(defn a*
  "https://en.wikipedia.org/wiki/A*_search_algorithm"
  [start end? heuristic explore]
  (loop [explored #{(:equipment start)}
         frontier (priority-map start 0)]
    (if (empty? frontier) :no-solution
        (let [[state priority] (peek frontier)
              neighbors        (filter (comp (complement explored) :equipment) (explore state))
              heuristics       (map heuristic neighbors)]
          (if (end? state) state
              (recur (into explored (map :equipment neighbors))
                     (into (pop frontier) (map vector neighbors heuristics))))))))

(defn line-formula
  ([y] (fn [[y' x']] (= y' y)))
  ([y x m] (fn [[y' x']] (= y' (- (* m x') (* m x) y)))))

(defn manhatten-distance [[x y] [x' y']]
  (+ (Math/abs (- x x')) (Math/abs (- y y'))))
