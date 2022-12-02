(ns advent-2015.day17
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input (map (fn [n] (Integer/parseInt n))
                (line-seq (io/reader (io/resource "2015/day17")))))

(defn ^:private sum? [n]
  (fn [coll] (= n (reduce + coll))))

(defn container-combinations [coll n]
  (into [] (comp (mapcat (partial combo/combinations (map-indexed vector coll)))
              (map (partial map second))
              (filter (sum? n)))
        (range (count coll))))

;; part 1
;; (count (container-combinations input 150))
;; => 4372

;; part 2
;; (second (apply min-key key (frequencies (map count (container-combinations input 150)))))
;; => 4

