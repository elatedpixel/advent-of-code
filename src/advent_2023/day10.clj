(ns advent-2023.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day10"))))
(def sample (str/trim (slurp (io/resource "2023/day10.sample"))))

(def rules
  {:N (set "S|7F")
   :S (set "S|LJ")
   :E (set "S-J7")
   :W (set "S-LF")})

(def opposite
  {:N :S :S :N :E :W :W :E})

(defn- pipes-connect? [from to dir]
  (and ((rules dir) to)
       ((rules (opposite dir)) from)))

(def offset
  {:N [-1 0]
   :E [0 1]
   :S [1 0]
   :W [0 -1]})

(defn- neighbor [coord dir]
  (mapv + coord (offset dir)))

(defn- connect
  [grid maze [coord node] & neighbors]
  (reduce
   (fn [m direction]
     (let [coord' (neighbor coord direction)
           pipe   (get-in grid coord' nil)]
       (if (pipes-connect? node pipe direction)
         (-> m
             (update coord (fnil conj #{}) coord')
             (update coord' (fnil conj #{}) coord))
         m)))
   maze
   neighbors))

(defn- pipe-maze
  "create adjacency matrix"
  [input]
  (let [maze-lines (mapv vec (str/split-lines input))
        connect    (partial connect maze-lines)]
    (reduce
     (fn [maze [coord col :as node]]
       (case col
         \| (connect maze node :N :S)
         \- (connect maze node :E :W)
         \L (connect maze node :N :E)
         \J (connect maze node :N :W)
         \7 (connect maze node :W :S)
         \F (connect maze node :E :S)
         \S (assoc maze :start coord)
         \. maze
         (throw (Error. "unexpected symbol in maze"))))
     {}
     (for [[i row] (map-indexed vector maze-lines)
           [j col] (map-indexed vector row)]
       [[i j] col]))))

(defn- explore [maze start]
  (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY [0 start])
         seen     {}]
    (if-let [[steps current] (peek frontier)]
      (let [neighbors (remove seen (maze current))]
        (recur (into (pop frontier) (map (partial vector (inc steps))) neighbors)
               (assoc seen current steps)))
      seen)))

(defn- draw [steps]
  (let [rows (inc (apply max (map first (keys steps))))
        cols (inc (apply max (map second (keys steps))))]
    (println
     (str/join
      \newline
      (for [i (range rows)]
        (apply str (for [j (range cols)] (get steps [i j] ".")))))
     \newline)))

(defn- draw-map [s]
  (println (str/join \newline (str/split-lines s))
           \newline))

(defn- part-1 [input]
  (let [maze  (pipe-maze input)
        steps (explore maze (:start maze))]
    steps))

(test/deftest test-part-1
  (test/is (= 8 (apply max (vals (part-1 sample)))))
  (test/is (= 6613 (apply max (vals (part-1 input))))))

(defn- part-2 [input]
  (let [maze  (pipe-maze input)
        steps (explore maze (:start maze))]
    ;; TODO: count area of maze bounded by pipes
    4))
