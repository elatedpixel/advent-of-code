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

(defn intcode [{:keys [program ptr in out halted?]}]
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
      4  (do
           (println "outputting" (value (inc p) (modes 0)))
           (go (>! out (value (inc p) (modes 0))))
           (dosync
            (alter ptr + 2)))
      99 (dosync (ref-set halted? true)
                 (alter ptr inc)))))

(defprotocol Computer
  (run [this])
  (input [this value]))

(defrecord Intcode [program ptr in out halted?]
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
  (->Intcode (ref intcode) (ref ptr) in out (ref false)))

