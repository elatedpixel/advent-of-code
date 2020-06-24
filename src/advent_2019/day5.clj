(ns advent-2019.day5
  (:require [advent-2019.core :refer [read-lines]]
            [advent-2019.intcode-async :refer (make-intcode run input output)]
            [clojure.core.async :refer (<!!)]
            #_[advent-2019.intcode :refer [computer]]
            [clojure.test :as t]))

(def data
  (->> (read-lines "2019/day5.txt")
       first
       (format "(%s)")
       read-string
       vec))

(defn -main []
  (time (run (input (make-intcode data) 1))))

(t/deftest test-simple-program
  ; output whatever the input is
  (t/is (= 42 (<!! (:out (run (input (make-intcode [3 0 4 0 99]) 42))))))

  ; create halt instruction with multiplication
  (t/is (= [1002 4 3 4 99] @(:program (run (input (make-intcode [1002 4 3 4 33]) 0))))))

(t/run-tests 'advent-2019.day5)
