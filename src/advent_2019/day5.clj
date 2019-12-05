(ns advent-2019.day5
  (:require [advent-2019.core :refer [read-lines]]
            [clojure.test :as t]))

(defn opcode [instruction]
  (rem instruction 100))

(defn instruction-size [instruction]
  (case (opcode instruction)
    1 4
    2 4
    3 2
    4 2
    99 1))

(defmulti execute 
  (fn [state [instruction & _] _]
    (opcode instruction)))

(defmethod execute 2
  [state [instruction a b address] parameter]
  )

(t/with-test

  (defn computer [{:keys [parameter instruction-pointer program-state]
                  :as state'}]
    (let [instruction (program-state instruction-pointer)
          n (instruction-size instruction)]
      
      (-> state'
          (update :program-state execute (subvec program-state instruction-pointer n) parameter)
          (update :instruction-pointer #(+ % n)))))

  (t/is (= [1002 4 3 4 99] (computer {:parameter 0
                                     :instruction-pointer 0
                                     :program-state [1002 4 3 4 33]})))

  ; computer
  )

(t/run-tests 'advent-2019.day5)

(def input
  (->> (read-lines "2019/day5.txt")
       first
       (format "(%s)")
       read-string
       vec))

(def state
  {:parameter 0
   :instruction-pointer 0
   :program-state input})
