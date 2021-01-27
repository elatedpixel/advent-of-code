(ns advent-2020.day17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def cell
  {\# true
   \. false})

(defn conway [cell neighbors]
  (or (and cell (<= 2 neighbors 3))
      (and (not cell) (= 3 neighbors))))

(defn neighbors [[z y x]]
  (for [dx    (range -1 2)
        dy    (range -1 2)
        dz    (range -1 2)
        :when (not= 0 dx dy dz)]
    (map + [dz dy dx] [z y x])))

(defn evolve [game]

  (transduce
   (comp
    (mapcat neighbors)
    (distinct))
   (completing
    (fn [m coord]
      (update m coord conway
              (count (sequence (comp (map game) (filter true?))
                               (neighbors coord))))))
   game
   (keys game)))

(def sample-input
  ".#.
..#
###")

(defn parse-input [s]
  (reduce
   (fn [m [coord cell]] (assoc m coord cell))
   {}
   (for [[y row] (map list (range) (str/split-lines s))
         [x col] (map list (range) (map cell (seq row)))]
     [[0 y x] col])))

;; part 1
(comment
  (time
   (println
    (->> (slurp (io/resource "2020/day17"))
         parse-input
         (iterate evolve)
         (take 7)
         last
         vals
         (filter true?)
         count)))
;; 213
;; "Elapsed time: 1404.630115 msecs"
  )

(defn neighbors-4d [[w z y x]]
  (for [dx    (range -1 2)
        dy    (range -1 2)
        dz    (range -1 2)
        dw    (range -1 2)
        :when (not= 0 dx dy dz dw)]
    (map + [dw dz dy dx] [w z y x])))

(defn parse-input-4d [s]
  (reduce
   (fn [m [coord cell]] (assoc m coord cell))
   {}
   (for [[y row] (map list (range) (str/split-lines s))
         [x col] (map list (range) (map cell (seq row)))]
     [[0 0 y x] col])))

;; part 2
(comment
  (with-redefs [neighbors neighbors-4d]
    (time
    (println
     (->> (slurp (io/resource "2020/day17"))
          parse-input-4d
          (iterate evolve)
          (take 7)
          last
          vals
          (filter true?)
          count))))
  ;; 1624
  ;; "Elapsed time: 48127.067498 msecs"
  )
