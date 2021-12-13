(ns advent-2021.day11
  (:require [advent.core :as c]
            [clojure.set :refer (difference)]))

(defn- to-hashmap [coll]
  (into {}
        (for [i (range (count coll))
              j (range (count (coll i)))]
          [[i j] ((coll i) j)])))

(def input (to-hashmap (mapv (partial mapv (comp read-string str))
                             (c/load-input 2021 11 #_{:test true}))))

(defn- neighbors [[x y]]
  (for [dx    (range -1 2)
        dy    (range -1 2)
        :when (not= 0 dx dy)]
    (map + [dx dy] [x y])))

(def ^:private charged? #(> % 9))

(defn- increase-energy [octopodes]
  (persistent!
   (reduce (fn [octopodes [k v]] (assoc! octopodes k (inc v)))
           (transient octopodes)
           octopodes)))

(defn- flash [octopodes charged]
  (reduce (fn [octopodes coord]
            (reduce (fn [octopodes coord] (update octopodes coord #(if (zero? %) % (inc %))))
                    (assoc octopodes coord 0)
                    (filter #(contains? octopodes %)
                            (neighbors coord))))
          octopodes
          charged))

(defn- step [octopodes]
  (loop [octopodes (increase-energy octopodes)
         charged (set (c/filter-key-by-val charged? octopodes))
         flashed #{}]
    (if (empty? charged)
      [octopodes flashed]
      (let [octopodes (flash octopodes charged)]
        (recur octopodes
               (difference (set (c/filter-key-by-val charged? octopodes))
                           charged
                           flashed)
               (into flashed charged))))))

(defn- simulate [octopodes]
  (iterate (fn [[octopodes flashed]] (step octopodes))
           [octopodes nil]))

(transduce (map (comp count second))
           +
           (take 100 (rest (simulate input))))
;; => 1625

(defn- synchronize [octopodes]
  (let [n (count octopodes)]
    (some (fn [[i [_ flashed]]]
            (when (= n (count flashed)) i))
          (map vector (range) (simulate octopodes)))))

(time (synchronize input))
;; => 244
;; "Elapsed time: 65.7191 msecs"
