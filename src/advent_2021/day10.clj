(ns advent-2021.day10
  (:require [advent.core :refer (load-input string->sexpression)]))

(def input (load-input 2021 10))

(def error (into {} (map vector ")]}>" [3 57 1197 25137])))

(def close (into {} (map vector "([{<" ")]}>")))

(defn check-syntax [stack character]
  (if (#{\) \] \} \>} character)
    (if (= character (close (peek stack)))
      (pop stack)
      (reduced {:corrupted (error character)}))
    (conj stack character)))

(transduce
  (comp (map (partial reduce check-syntax []))
        (filter map?)
        (map :corrupted))
  +
  input)
;; => 366027

(def autocomplete
  (into {} (map vector ")]}>" [1 2 3 4])))

(defn gold [total c]
  (+ (autocomplete (close c)) (* 5 total)))

(let [scores (sequence
              (comp
               (map (partial reduce check-syntax []))
               (filter (complement map?))
               (map reverse)
               (map (partial reduce gold 0)))
              input)]
  (time (nth (sort scores) (quot (count scores) 2))))
;; => 1118645287
;; "Elapsed time: 2.344609 msecs"
