(ns advent-2020.day05
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(t/with-test

  (defn decode-seat [boarding-pass]
    (let [[valid? row column] (re-matches #"([FB]{7})([LR]{3})" boarding-pass)]
      [(Integer/parseInt (s/replace row #"F|B" {"F" "0" "B" "1"}) 2)
       (Integer/parseInt (s/replace column #"L|R" {"L" "0" "R" "1"}) 2)])
    )

  (t/is (= [44 5] (decode-seat "FBFBBFFRLR")))
  (t/is (= [70 7] (decode-seat "BFFFBBFRRR")))
  (t/is (= [14 7] (decode-seat "FFFBBBFRRR")))
  (t/is (= [102 4] (decode-seat "BBFFBBFRLL"))))

(def input
  (line-seq (io/reader (io/resource "2020/day05"))))

;; part 1
(comment
  (time (println (apply max-key (fn [[y x]] (+ x (* y 8)))
                          (map decode-seat input)))))

(t/run-tests 'advent-2020.day05)
