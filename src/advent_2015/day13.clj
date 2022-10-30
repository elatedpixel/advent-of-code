(ns advent-2015.day13
  (:require
   [clojure.string :as str]
   [advent.core :as c]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]))

(defn parse [input]
  (let [re #"(?<a>\w+) would (?<g>gain|lose) (?<n>\d+) happiness units by sitting next to (?<b>\w+)\."]
   (map (comp #(map read-string (rest (re-matches re %))))
        (str/split-lines input))))

(defn to-map [m [a g n b]]
  (assoc-in m [a b] (case g
                      'gain n
                      (* -1 n))))

(defn happiness [input]
  (reduce to-map {} (parse input)))

(defn joy [happy seating]
  (letfn [(score [a b] (+ (get-in happy [a b])
                          (get-in happy [b a])))]
    (reduce + (map score seating (rest (cycle seating))))))

(defn bliss [happy]
  (apply max (map (partial joy happy) (combo/permutations (keys happy)))))

(bliss (happiness (slurp (io/resource "2015/day13"))))
;; => 733

(defn add-myself [m]
  (reduce-kv (fn [m k v] (-> m
                             (assoc-in ['myself k] 0)
                             (assoc-in [k 'myself] 0)))
             m
             m))

(bliss (add-myself (happiness (slurp (io/resource "2015/day13")))))
;; => 725
