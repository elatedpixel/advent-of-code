(ns advent-2015.day01
  (:require [clojure.test :as test]
            [clojure.java.io :as io]))

(test/with-test
  (defn floor [input]
    (let [instructions (frequencies input)]
      (- (instructions \() (instructions \)))))

  (test/is (= -1 (floor "))(")))
  (test/is (= 3 (floor "))((((("))))

(time (println "day 1 part 1" (floor (slurp (io/resource "2015/day01")))))

(def elevator {\( 1
               \) -1})

(test/with-test
  (defn first-basement-trip [input]
    (first
     (reduce
      (fn [[i x] c]
        (if (neg? x) (reduced [i x])
            [(inc i) (+ x (elevator c))]))
      [0 0]
      input)))

  (test/is (= 1 (first-basement-trip ")")))
  (test/is (= 5 (first-basement-trip "()())"))))

(time (println "day 1 part 2" (first-basement-trip (slurp (io/resource "2015/day01")))))

#_(test/run-tests 'advent-2015.day01)
