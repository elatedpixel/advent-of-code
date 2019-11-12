(ns advent-2016.day2
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def data
  (->> "2016/day2.txt"
       io/resource
       io/reader
       line-seq))

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def keypad2
  [[0 0 1 0 0]
   [0 2 3 4 0]
   [5 6 7 8 9]
   [0 \A \B \C 0]
   [0 0 \D 0 0]])

(t/with-test
  (defn move [[row col] direction]
    (case direction
      \L [row (max 0 (dec col))]
      \R [row (min 2 (inc col))]
      \U [(max 0 (dec row)) col]
      \D [(min 2 (inc row)) col]))
  (t/is (= [0 0] (move [0 0] \L)))
  (t/is (= [2 0] (move [1 0] \D)))
  (t/is (= [2 0] (move [2 0] \D))))

(defn move2 [[row col] direction]
  (let [position
        (case direction
          \L [row (max 0 (dec col))]
          \R [row (min 4 (inc col))]
          \U [(max 0 (dec row)) col]
          \D [(min 4 (inc row)) col])]
    (if (= 0 (get-in keypad2 position))
      [row col] position)))

(defn part1 [data]
  (loop [password []
         position [1 1]
         instructions data]
    (if (nil? instructions) password
        (let [position' (reduce move position (first instructions))]
          (recur (conj password (get-in keypad position'))
                 position'
                 (next instructions))))))

(defn part2 [data]
  (loop [password []
         position [1 1]
         instructions data]
    (if (nil? instructions) password
        (let [position' (reduce move2 position (first instructions))]
          (recur (conj password (get-in keypad2 position'))
                 position'
                 (next instructions))))))

(defn -main []
  (time (prn (part1 data)))
  (time (prn (part2 data))))

(t/run-tests 'advent-2016.day2)
