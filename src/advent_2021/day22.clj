(ns advent-2021.day22
  (:require [clojure.string :refer (split-lines)]
            [clojure.core.reducers :as r]
            [advent.core :as c]))


(defn silver [reactor]
  (reduce + 0
          (for [x (range -50 (inc 50))
                y (range -50 (inc 50))
                z (range -50 (inc 50))
                :when (contains? reactor [x y z])]
            1)))

(defn instruction [s]
  (map read-string (rest (re-find #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" s))))

(defn constrained-range [m M]
  (fn [a b] (range (max m a) (min M b))))

(defn execute [reactor [mode x1 x2 y1 y2 z1 z2]]
  (let [range' (constrained-range -50 (inc 50))
        cubes (for [x (range' x1 (inc x2))
                    y (range' y1 (inc y2))
                    z (range' z1 (inc z2))]
                [x y z])]
    (case mode
      on  (apply conj reactor cubes)
      off (apply disj reactor cubes))))

(def example "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682")

(def input (mapv instruction (c/load-input 2021 22)))

#_(time (count (reduce execute #{} input)))
;; => 587097

(defn gold [input]
  (letfn [(combine
            ([] {})
            ([a b] (merge-with + a b)))

          (reducer
            ([] {})
            ([r [mode x1 x2 y1 y2 z1 z2]]
             (reduce
               (fn [m [k v]] (update m k (fnil + v)))
               r
               (for [x (range x1 (inc x2))
                     y (range y1 (inc y2))
                     z (range z1 (inc z2))]
                 [[x y z] (case mode on 1 off -1)]))))]
    (r/fold combine reducer input)))

(time
  (transduce
    (comp
      (map val)
      (filter (every-pred pos? odd?)))
    +'
    (gold (take 100 input))))
