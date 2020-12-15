(ns advent-2020.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def input (slurp (io/resource "2020/day11")))

(defn- parse-seating-map [s]
  (into [] (map vec) (str/split s #"\n")))

(defn- apply-seating-rule [seat neighbors]
  (cond
    (and (= \L seat) (zero? neighbors)) \#
    (and (= \# seat) (>= neighbors 4))  \L
    :else                               seat))

(defn- neighbors [seats coord]
  (transduce (comp (keep (fn [offset] (get-in seats (mapv + coord offset))))
                   (map {\. 0 \L 0 \# 1}))
             +
             (for [i (range -1 2)
                   j (range -1 2)
                   :when (not= 0 i j)] [i j])))

(defn- seats-reducer [seats [coord new-value]]
  (assoc-in seats coord new-value))

(defn- musical-chairs [seats]
  (let [rows (count seats)
        cols (count (first seats))]
    (reduce
     seats-reducer
     seats
     (for [i (range rows)
           j (range cols)
           :let [seat (get-in seats [i j])
                 neighbor-count (neighbors seats [i j])]
           :when (not= \. seat)]
       [[i j] (apply-seating-rule seat neighbor-count)]))))

(defn- count-seats [seat-type seats]
  (count (filter #{seat-type} (flatten seats))))

(defn- musical-chairs-stabilize [seats]
  (loop [seats          seats
         seating-changes []]
    (let [seats' (musical-chairs seats)]
      (if (= seats seats')
        seating-changes
        (recur seats' (conj seating-changes seats'))))))

;; part 1
(comment
  (count-seats \# (last (musical-chairs-stabilize (parse-seating-map input)))))

(defn los [seats [y x] [dy dx]]
  ({nil 0 \L 0 \# 1}
   (some #{\L \#}
         (sequence
          (comp
           (map #(get-in seats %))
           (take-while (complement nil?)))
          (rest (iterate #(mapv + % [dy dx]) [y x]))))))

(defn- line-of-sight-neighbors [seats coord]
  (transduce
   (map (fn [delta]
          (los seats coord delta)))
   +
   0
   (for [i (range -1 2)
         j (range -1 2)
         :when (not= 0 i j)] [i j])))

(defn- apply-tolerant-seating-rule [seat neighbors]
  (cond
    (and (= \L seat) (zero? neighbors)) \#
    (and (= \# seat) (>= neighbors 5))  \L
    :else                               seat))

;; part 2
(comment
  (with-redefs [neighbors          line-of-sight-neighbors
               apply-seating-rule apply-tolerant-seating-rule]
    (time
     (println (count-seats \# (last (musical-chairs-stabilize (parse-seating-map input))))))))
