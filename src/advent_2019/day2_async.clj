(ns advent-2019.day2-async
  (:require [clojure.java.io :as io]
            [clojure.test :as t]
            [clojure.core.async
             :as async
             :refer (chan go go-loop >! <! >!! <!!)]))

(defn input->map [d]
  (zipmap (range) d))

(def input
  (->> "2019/day2.txt"
       io/resource
       io/reader
       line-seq
       first
       (format "(%s)")
       read-string
       input->map))

(defn sort-by-keys [m]
  (map m (sort (keys m))))

(defn arguments [program ptr op]
  (let [indexes (iterate inc ptr)]
    (case op
      (1 2) (vals (select-keys program (rest (take 4 indexes))))
      99    '())))

(defn intcode [{:keys [program ptr in out halted?]}]
  (let [op   (get @program @ptr)
        args (arguments @program @ptr op)]
    (case op
      1 (let [[a b c] args]
          (dosync
           (alter program assoc c (+ (@program a) (@program b)))
           (alter ptr + (inc (count args)))))
      2 (let [[a b c] args]
          (dosync
           (alter program assoc c (* (@program a) (@program b)))
           (alter ptr + (inc (count args)))))
      99 (dosync (ref-set halted? true)
                 (alter ptr inc)))))

(defprotocol Computer
  (run [this]))

(defrecord Intcode [program ptr in out halted?]
  Computer
  (run [this]
    (while (not @(:halted? this))
      (intcode this))
    this))

(defn make-intcode
  [intcode & {:keys [ptr in out]
              :or   {ptr 0
                     in  (chan)
                     out (chan)}}]
  (->Intcode (ref intcode) (ref ptr) in out (ref false)))

(t/deftest test-intcode
  (t/are [input expect] (= expect
                           (-> input
                               input->map
                               make-intcode
                               run
                               :program
                               deref
                               sort-by-keys))
    [1 0 0 0 99] [2 0 0 0 99]
    [2 3 0 3 99] [2 3 0 6 99]
    [2 4 4 5 99 0] [2 4 4 5 99 9801]
    [1 1 1 4 99 5 6 0 99] [30 1 1 4 2 5 6 0 99])
                                        ;
  )

(comment
                                        ;
  (def computer-a (make-intcode (-> input
                                    (assoc 1 12)
                                    (assoc 2 2))))
                                        ;
  (run computer-a)

  ((:program computer-a) 0)
                                        ;
  (>!! (:in computer-a) 0)
                                        ;
  (println (<!! (:out computer-a)))
                                        ;
  (for [noun (range 100)
        verb (range 100)
        :when (= 19690720
                 ((:program (run (make-intcode (-> input (assoc 1 noun) (assoc 2 verb))))) 0))]
    [noun verb]
    )
  )
