(ns advent-2016.day8
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.test :as t]))

;; domain ***********************************
(defn rotate [coll n]
  (let [width (count coll)]
    (take width (drop (- width n) (cycle coll)))))

(defprotocol Screen
  (out [this])
  (debug [this])
  (rect [this x y])
  (rotate-row [this y n])
  (rotate-column [this x n]))

(defrecord LCD [pixels height width]
  Screen
  (out [this]
    (doseq [s (mapv #(apply str %) (debug this))]
      (println s)))
  (debug [this]
    (mapv vec pixels))
  (rect [this cols rows]
    (doseq [x (range cols)
            y (range rows)]
      (aset pixels y x 1))
    this)
  (rotate-row [this y n]
    (let [rotated (rotate (get (debug this) y) n)]
      (doseq [x (range width)]
       (aset pixels y x (nth rotated x)))))
  (rotate-column [this x n]
    (let [rotated (rotate (mapv #(get % x) (debug this)) n)]
      (doseq [y (range height)]
        (aset pixels y x (nth rotated y))))))

(defn make-LCD [height width]
  (->LCD (make-array Long/TYPE height width) height width))

;; run code with domain *********************
(def HEIGHT 6)
(def WIDTH 50)

(def data
  (-> "2016/day8.txt"
      io/resource
      io/reader
      line-seq))

(defn parse-int [s]
  (try (Integer/parseInt s)
       (catch Exception e (str "Caught " (.getMessage e) ": for string " s))))

(defn parse-instruction [s]
  (let [[parsed? fun a b]
        (re-matches #"^.*(rect|row|column)[\sxy=]+(\d+)[x\sby]+(\d+)" s)
        a' (parse-int a)
        b' (parse-int b)]
    (case fun
      "rect" #(rect % a' b')
      "row" #(rotate-row % a' b')
      "column" #(rotate-column % a' b')
      (println s))))

(let [lcd (make-LCD HEIGHT WIDTH)]
  (doseq [d data]
    (try
      ((parse-instruction d) lcd)
      (catch Exception e (println (str (.getMessage e) ": " d)))))
  (println (count (filter #(= 1 %) (flatten (debug lcd))))) ; part 1
  (out lcd))                                                ; part 2
