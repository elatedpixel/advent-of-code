(ns day3.core
  (:require [clojure.test :refer [with-test is run-tests]]))

(with-test

  (defn parse-plan [plan]
    (->> (re-matches #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" plan)
         rest
         (map read-string)
         vec))

  (is (= [1 1 3 4 4] (parse-plan "#1 @ 1,3: 4x4")))
  (is (= [12 123 304 40 154] (parse-plan "#12 @ 123,304: 40x154"))))

(with-test

  (defn find-overlapping [plans]
    (count (filter (fn [[k v]] (> (count v) 1))
            (reduce (fn [m [elf col row width height]]
                      (reduce (fn [m [c r]] (update m [c r] #(conj % elf))) m
                              (for [c (range col (+ col width))
                                    r (range row (+ row height))]
                                [c r])))
                    {}
                    plans))))

  (is (= 4 (find-overlapping  (map parse-plan ["#1 @ 1,3: 4x4"
                                               "#2 @ 3,1: 4x4"
                                               "#3 @ 5,5: 2x2"])))))

(run-tests 'day3.core)

(defn -main [file-name]
  (let [input (map parse-plan (clojure.string/split-lines (slurp file-name)))]
    (prn)
    (println (str "Day 3 Part 1: " (time (find-overlapping input))))))
