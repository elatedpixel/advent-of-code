(ns advent-2019.intcode-async
  (:require [clojure.test :as t]
            [clojure.core.async
             :as async
             :refer (chan go go-loop >! <! >!! <!!)]))

(defn input->map [d]
  "convert collection into map where keys are indexes"
  (zipmap (range) d))

(defn sort-by-keys [m]
  "return values in map sorted by keys"
  (map m (sort (keys m))))

(defn instruction [n]
  [(rem n 100) (vec (reverse (format "%03d" (quot n 100))))])

(defn value-by-mode [program index mode]
  (case mode
    \0 (program (program index))
    \1 (program index)))

(def bool->int
  {true  1
   false 0
   nil   0})

(defn intcode [{:keys [program ptr diagnostic in out halted?]}]
  (let [p          @ptr
        [op modes] (instruction (program p))
        value      (fn [i m] (value-by-mode @program i m))]
    (case op
      1  (dosync
          (alter program assoc (program (+ 3 p))
                 (+ (value (+ 1 p) (modes 0))
                    (value (+ 2 p) (modes 1))))
          (alter ptr + 4))
      2  (dosync
          (alter program assoc (program (+ 3 p))
                 (* (value (+ 1 p) (modes 0))
                    (value (+ 2 p) (modes 1))))
          (alter ptr + 4))
      3  (dosync
          (alter program assoc (program (inc p)) (<!! in))
          (alter ptr + 2))
      4  (dosync
          (ref-set diagnostic (value (inc p) (modes 0)))
          (alter ptr + 2))
      5  (dosync
          (if (not (zero? (value (inc p) (modes 0))))
            (ref-set ptr (value (+ 2 p) (modes 1)))
            (alter ptr + 3)))
      6  (dosync
          (if (zero? (value (inc p) (modes 0)))
            (ref-set ptr (value (+ 2 p) (modes 1)))
            (alter ptr + 3)))
      7  (dosync
          (alter program assoc (program (+ 3 p))
                 (bool->int (< (value (+ 1 p) (modes 0))
                               (value (+ 2 p) (modes 1)))))
          (alter ptr + 4))
      8  (dosync
          (alter program assoc (program (+ 3 p))
                 (bool->int (= (value (+ 1 p) (modes 0))
                               (value (+ 2 p) (modes 1)))))
          (alter ptr + 4))
      99 (dosync (ref-set halted? true)
                 (go (>! out @diagnostic))
                 (alter ptr inc)))))

(defprotocol Computer
  (run [this])
  (input [this value]))

(defrecord Intcode [program ptr diagnostic in out halted?]
  Computer
  (run [this]
    (while (not @(:halted? this))
      (intcode this))
    this)
  (input [this value]
    (go (>! (:in this) value))
    this))

(defn make-intcode
  [intcode & {:keys [ptr in out]
              :or   {ptr 0
                     in  (chan)
                     out (chan)}}]
  (->Intcode (ref intcode) (ref ptr) (ref 0) in out (ref false)))

