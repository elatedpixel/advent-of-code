(ns advent-2016.day1
  (:require
   [clojure.test :refer [is testing with-test run-tests]]
   [clojure.java.io :as io]
   [clojure.string :as s]))

(def turn
  {:north {\R :east \L :west}
   :south {\R :west \L :east}
   :west  {\R :north \L :south}
   :east  {\R :south \L :north}})

(def move
  {:north [0 1]
   :east  [1 0]
   :south [0 -1]
   :west  [-1 0]})

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn moves [position direction n]
  (->> position
       (iterate #(map + % (move direction)))
       (drop 1)
       (take n)))

(with-test
  (def parse-instruction
    (juxt first #(Integer/parseInt (apply str (rest %)))))
  (is (= [\R 103] (parse-instruction "R103"))))

(defn day1 [data]
  (distance
   (:position
    (last
     (reductions
      (fn [{:keys [facing position] :as state}
           [delta n-moves]]
        (let [direction ((turn facing) delta)
              steps (moves position direction n-moves)]
          (-> state
              (assoc :facing direction)
              (assoc :position (last steps)))))
      {:facing :north :position [0 0]}
      (map parse-instruction data))))))

(defn day2 [data]
  (distance
   (:position
    (last
     (reductions
      (fn [{:keys [facing position seen?] :as state}
           [delta n-moves]]
        (let [direction ((turn facing) delta)
              steps (moves position direction n-moves)]
          (if-let [seen (some seen? steps)]
            (reduced {:position seen})
            (-> state
               (assoc :seen? (apply conj seen? steps))
               (assoc :facing direction)
               (assoc :position (last steps))))))
      {:facing :north :position [0 0] :seen? #{}}
      (map parse-instruction data))))))

(def data (s/split (slurp (io/resource "2016/day1.txt")) #"\W+"))

(defn -main []
  (time (println (day1 data)))
  (time (println (day2 data))))

#_(run-tests 'advent-2016.day1)
