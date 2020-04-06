(ns advent-2019.day2-async
  (:require [clojure.java.io :as io]
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
      1 (vals (select-keys program (rest (take 4 indexes)))))))

(defn intcode [{:keys [program ptr in]} input]
  (let [op   (get @program @ptr)
        args (arguments @program @ptr op)]
    (case op
      1 (let [[a b c] args]
          (dosync
           (alter program assoc c (+ a b))
           (alter ptr + (inc (count args))))))))

(defprotocol Computer
  (run [this]))

(defrecord Intcode [program ptr in out]
  Computer
  (run [this]
    #_(intcode this 0)
    (go
        (>! (:out this) (intcode this (<! (:in this)))))))

(defn make-intcode
  [intcode & {:keys [ptr in out]
              :or   {ptr 0
                     in  (chan)
                     out (chan)}}]
  (->Intcode (ref intcode) (ref ptr) in out))

(comment
                                        ;
  (def computer-a (make-intcode input))
                                        ;
  (run computer-a)
                                        ;
  (>!! (:in computer-a) 0)
                                        ;
  (println (<!! (:out computer-a)))
                                        ;
  )
