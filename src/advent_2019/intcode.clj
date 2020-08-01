(ns advent-2019.intcode
  (:require [clojure.test :as t]
            [clojure.core.async :as a :refer [<! >! chan]]))

(defn instruction [n]
  [(rem n 100) (vec (reverse (format "%03d" (quot n 100))))])

(defn execute [{:keys [program pointer input] :as state}]
  (let [[op modes] (instruction (program pointer))
        value      (fn [i m] (case m
                               \0 (program (program i))
                               \1 (program i)))]
    (case op
      1 (-> (assoc-in state [:program (program (+ 3 pointer))]
                      (+ (value (+ 1 pointer) (modes 0))
                         (value (+ 2 pointer) (modes 1))))
            (update :pointer + 4))
      2 (-> (assoc-in state [:program (program (+ 3 pointer))]
                      (* (value (+ 1 pointer) (modes 0))
                         (value (+ 2 pointer) (modes 1))))
            (update :pointer + 4))
      3 (-> (assoc-in state [:program (value (+ 1 pointer) (modes 0))] (first input))
            (assoc :input (vec (rest input)))
            (update :pointer + 2))
      4 (-> (assoc state :output (value (+ 1 pointer) (modes 0)))
            (update :pointer + 2))
      5 (-> (assoc state :pointer (if ((complement zero?) (value (+ pointer 1) (modes 0)))
                                    (value (+ pointer 2) (modes 1))
                                    (+ 3 pointer))))
      6 (-> (assoc state :pointer (if (zero? (value (+ pointer 1) (modes 0)))
                                    (value (+ pointer 2) (modes 1))
                                    (+ 3 pointer))))
      7 (-> (assoc-in state [:program (program (+ 3 pointer))]
                      (if (< (value (+ 1 pointer) (modes 0))
                             (value (+ 2 pointer) (modes 1))) 1 0))
            (update :pointer + 4))
      8 (-> (assoc-in state [:progam (program (+ 3 pointer))]
                      (if (= (value (+ 1 pointer) (modes 0))
                             (value (+ 2 pointer) (modes 1))) 1 0))
            (update :pointer + 4))
      (assoc state :halt? true))))

(t/with-test

  (defn computer [{:keys [program pointer halt?] :as state}]
    (last (take-while (complement :halt?) (iterate execute state))))

  (t/is (= [1002 4 3 4 99]
           (:program
            (computer {:halt?   false
                       :input   '()
                       :output  '()
                       :pointer 0
                       :program [1002 4 3 4 33]}))))

  ; computer
  )

(t/run-tests 'advent-2019.intcode)
