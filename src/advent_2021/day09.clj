(ns advent-2021.day09
  (:require [advent.core :refer (load-input)]
            [clojure.string :as s]))

(def input (load-input 2021 9))

(defn parse [string]
  (mapv (partial mapv (comp read-string str)) string))

(defn lava-tubes->map [coll]
  (into {}
        (apply
         concat
         (map-indexed
          (fn [i row]
            (map-indexed (fn [j col] [[i j] col]) row)) coll))))

(defn offsets [[i j]]
  [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]])

(def lava-tubes (lava-tubes->map (parse input)))
(def neighbors
  (reduce
   (fn [m key]
     (assoc m key (filter (partial contains? lava-tubes)
                          (offsets key))))
   {}
   (keys lava-tubes)))

(defn low-points []
  (for [[pos nearest] neighbors
        :let [x (get lava-tubes pos)]
        :when (every? #(< x %) (map lava-tubes nearest))]
    [pos x]))

(defn risk [low-points]
  (transduce (comp (map second) (map inc)) + 0 low-points))

(risk (low-points))
;; => 607

(defn basin [pos]
  (loop [seen     #{pos}
         frontier (neighbors pos)
         result   #{pos}]
    (if (empty? frontier)
      result
      (let [expand (filter #(< (get lava-tubes %) 9) frontier)]
        (recur (into seen frontier)
               (filter (complement seen) (mapcat neighbors expand))
               (into result expand))))))

(->> (low-points)
     (map (comp count basin first))
     (sort >)
     (take 3)
     (reduce * 1))
;; => 900864
