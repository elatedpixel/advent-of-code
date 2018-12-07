(ns advent-2018.day1
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def coerce-input (comp read-string (partial format "(%s)")))

(t/with-test

  (defn calibrate-frequencies [frequencies]
    (reduce + frequencies))

  (t/is (= 3 (calibrate-frequencies (coerce-input "+1, +1, +1"))))
  (t/is (= 0 (calibrate-frequencies (coerce-input "+1, +1, -2"))))
  (t/is (= -6 (calibrate-frequencies (coerce-input "-1, -2, -3")))))

(t/with-test

  #_(defn find-repeat-frequency [frequencies]
    (loop [list (cycle frequencies)
           seen #{}
           val 0]
      (if (seen val) val
          (recur (rest list) (conj seen val) (+ val (first list))))))

  ;; https://github.com/bhauman/advent-of-clojure/blob/master/src/advent-2018/day01.clj
  ;; thanks Bruce - learning a lot from you
  (defn find-repeat-frequency [frequencies]
    (reduce
     #(if (%1 %2) (reduced %2) (conj %1 %2))
     #{0}
     (reductions + (cycle frequencies))))

  (t/is (= 0 (find-repeat-frequency (coerce-input "+1, -1"))))
  (t/is (= 10 (find-repeat-frequency (coerce-input "+3, +3, +4, -2, -4"))))
  (t/is (= 5 (find-repeat-frequency (coerce-input "-6, +3, +8, +5, -6"))))
  (t/is (= 14 (find-repeat-frequency (coerce-input "+7, +7, -2, -7, -4")))))


(t/run-tests 'advent-2018.day1)

(defn -main [input-file]
  (let [input (coerce-input (slurp (io/resource "2018/day1.txt")))]
    (prn)
    (println (str "Day 1 Part 1: " (time (calibrate-frequencies input))))
    (println (str "Day 1 Part 2: " (time (find-repeat-frequency input))))))
