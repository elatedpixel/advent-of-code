(ns advent-2024.day14
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defrecord Robot [x y dx dy])

(defn str->Robot [s]
  (let [[x y dx dy] (sequence (map parse-long) (re-seq #"-?\d+" s))]
    (Robot. x y dx dy)))

(defn- robots [file]
  (sequence
   (map str->Robot)
   (line-seq (io/reader (io/resource file)))))

(defn- robot-grid [robots]
  (reduce
   (fn [m {:keys [x y]}]
     (update m [x y] (fnil inc 0)))
   {}
   robots))

(def ^:private sample0 (robots "2024/day14.sample0"))
(def ^:private puzzle (robots "2024/day14.txt"))

(defn- display [robots & {:keys [bounds] :or {bounds [101 103]}}]
  (let [grid (robot-grid robots)]
    (println
      (s/join \newline
              (mapv (fn [y] (reduce str (map (fn [x] (get grid [x y] \.))
                                             (range (bounds 0)))))
                    (range (bounds 1)))))
    (println)))

(defn- teleport [^Robot {:keys [x y dx dy] :as robot}
                 & {:keys [n bounds] :or {n 1 bounds [101 103]}}]
  (-> robot
      (assoc :x (mod (+ x (* dx n)) (bounds 0)))
      (assoc :y (mod (+ y (* dy n)) (bounds 1)))))

(defn- safety-factor [robots & {:keys [bounds] :or {bounds [101 103]}}]
  (let [grid (robot-grid robots)]
    (reduce
     (fn [total [quadrants robot-grid-elements]]
       (* total
          (if (every? false? quadrants)
            1
            (reduce + (map second robot-grid-elements)))))
     1
     (group-by (fn [[[x y] v]] [(and (< x (quot (bounds 0) 2))
                                     (< y (quot (bounds 1) 2)))
                                (and (> x (quot (bounds 0) 2))
                                     (> y (quot (bounds 1) 2)))
                                (and (< x (quot (bounds 0) 2))
                                     (> y (quot (bounds 1) 2)))
                                (and (> x (quot (bounds 0) 2))
                                     (< y (quot (bounds 1) 2)))])
               grid))))

(defn part-1 [puzzle]
  (safety-factor
    (map #(teleport % :n 100) puzzle)))

(defn part-2 [puzzle]
  (loop [i 0]
    (let [whatever-dude (map #(teleport % :n i) puzzle)]
      (if (> 15000 (reduce (fn [r {:keys [x y]}] (+ r (abs (- x 50)) (abs (- y 51)))) 0 whatever-dude))
        (do
          (display whatever-dude)
          i)
        (recur (inc i))))))

(comment
  (part-1 puzzle)
  ;; => 217132650
  )

(defn -main []
  (println (str "Day 14 Part 1: " (part-1 puzzle)))
  (println (str "Day 14 Part 2: " (part-2 puzzle))))
