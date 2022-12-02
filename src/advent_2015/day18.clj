(ns advent-2015.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def cell
  {\# true
   \. false})

(defn conway [cell neighbors _]
  (or (and cell (<= 2 neighbors 3))
      (and (not cell) (= 3 neighbors))))

(defn neighbors [[y x]]
  (for [dx    (range -1 2)
        dy    (range -1 2)
        :when (not= 0 dx dy)]
    (map + [dy dx] [y x])))

(defn- count-lit-neighbors
  [game coord]
  (count (sequence (comp (map game)
                      (filter true?))
                   (neighbors coord))))

(defn evolve [rules]
  (fn [game]
    (reduce
     (fn [m coord]
       (update m coord rules (count-lit-neighbors game coord) coord))
     game
     (keys game))))

(def input (slurp (io/resource "2015/day18")))

(defn parse-input [s]
  (into {}
        (for [[y row] (map-indexed vector (str/split-lines s))
              [x col] (map-indexed vector (map cell row))]
          [[y x] col])))

(defn animate [evolve lights steps]
  (nth (iterate evolve lights) steps))

(defn lights-on? [lights]
  (count (filter true? (vals lights))))

;; part 1
#_(lights-on? (animate (evolve conway) (parse-input input) 100))
;; => 1061

(defn conway-stuck [cell neighbors coord]
  (case coord
    ([0 0] [0 99] [99 0] [99 99]) true
    (conway cell neighbors coord)))

;; part 2
(lights-on? (animate (evolve conway-stuck) (parse-input input) 100))
;; => 1006
