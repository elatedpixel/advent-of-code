(ns advent-2021.day04
  (:require [advent.core :refer [string->sexpression]]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp (io/resource "2021/day04")))

(defn make-board [s]
  (map string->sexpression (s/split-lines s)))

(let [[numbers & bingo-boards-strs] (s/split input #"\n\n")]
  (def numbers (string->sexpression numbers))
  (def bingo-boards (map make-board bingo-boards-strs)))

(defn- bingo? [called-numbers]
  (letfn [(check [xs] (every? (partial contains? called-numbers) xs))]
    (fn [board]
      (or (some check board)
         (some check (apply map list board))))))

(defn- score [board called-numbers]
  (transduce
    (filter (complement (partial contains? called-numbers)))
    +
    (flatten board)))

(defn call-number [called-numbers n]
  (let [called (conj called-numbers n)]
    (if-let [winner (some #(when ((bingo? called) %) %) bingo-boards)]
      (reduced (* n (score winner called)))
      called)))

(defn silver [numbers]
  (reduce call-number #{} numbers))

;; silver
(silver numbers)
;; => 72770

(defn- win-last [numbers]
  (loop [[n & numbers] numbers
         boards (set bingo-boards)
         winners []
         called-numbers #{}]
    (if (empty? boards)
      (last winners)
      (let [called (conj called-numbers n)
            bingos (into #{} (keep #(when ((bingo? called) %) %)) boards)]
        (recur numbers
               (remove bingos boards)
               (into winners (mapv #(* n (score % called)) bingos))
               called)))))

(defn gold [numbers]
  (win-last numbers))

;; gold
(gold numbers)
;; => 13912
