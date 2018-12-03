(ns day1.core
  (:require [clojure.test :as t]))

(defn coerce-input [input-string]
  (read-string (str "(" input-string ")")))

(t/with-test

  (defn calibrate-frequencies [frequencies]
    (reduce + frequencies))

  (t/is (= 3 (calibrate-frequencies (coerce-input "+1, +1, +1"))))
  (t/is (= 0 (calibrate-frequencies (coerce-input "+1, +1, -2"))))
  (t/is (= -6 (calibrate-frequencies (coerce-input "-1, -2, -3")))))

(t/with-test

  (defn find-repeat-frequency [frequencies]
    (loop [list (cycle frequencies)
           seen #{}
           val 0]
      (if (seen val) val
          (recur (rest list) (conj seen val) (+ val (first list))))))

  (t/is (= 0 (find-repeat-frequency (coerce-input "+1, -1"))))
  (t/is (= 10 (find-repeat-frequency (coerce-input "+3, +3, +4, -2, -4"))))
  (t/is (= 5 (find-repeat-frequency (coerce-input "-6, +3, +8, +5, -6"))))
  (t/is (= 14 (find-repeat-frequency (coerce-input "+7, +7, -2, -7, -4")))))

(t/run-tests 'day1.core)

(defn -main [input-file]
  (let [input (coerce-input (slurp input-file))]
    (prn)
    (println (str "Day 1 Part 1: " (time (calibrate-frequencies input))))
    (println (str "Day 1 Part 2: " (time (find-repeat-frequency input))))))
