(ns advent-2021.day14
  (:require [advent.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-pair-insertions [s]
  (let [[_ pair insertion] (re-find #"(\w{2}) -> (\w)" s)]
    (vector (vec pair)
            (vector (first pair) (first insertion))
            (vector (first insertion) (second pair)))))

(def input (slurp (io/resource "2021/day14")))

(let [[polymer-template pair-insertions] (s/split input #"\n\n")]
  (def polymers (map vec (partition 2 1 polymer-template)))
  (def polymer-template (frequencies polymers))
  (def pair-insertions (map parse-pair-insertions (s/split-lines pair-insertions))))

(defn process-pair-insertion [pair-insertions]
  (fn [polymer-template _]
    (reduce (fn [delta [pair insert-left insert-right]]
              (if-let [count-pair (polymer-template pair)]
                (-> delta
                    (update pair (fnil - 0) count-pair)
                    (update insert-left (fnil +' 0) count-pair)
                    (update insert-right (fnil +' 0) count-pair))
                delta))
            polymer-template
            pair-insertions)))

(defn insert-pairs
  ([polymer-template n]
   (reduce (process-pair-insertion pair-insertions) polymer-template (range n))))

(defn count-elements [polymer-template]
  (let [[a _] (first polymers)
        [_ b] (last polymers)]
    (into {}
          (map (fn [[k v]] [k (quot v 2)]))
          (->
           (reduce
            (fn [m [[a b] v]] (-> m
                                  (update a (fnil + 0) v)
                                  (update b (fnil + 0) v)))
            {}
            polymer-template)
           (update a (fnil + 0) 1)
           (update b (fnil + 0) 1)))))

(apply -
       (apply (juxt max min)
              (vals (count-elements (insert-pairs polymer-template 10)))))
;; => 3831

(apply -
       (apply (juxt max min)
              (vals (count-elements (insert-pairs polymer-template 40)))))
;; => 5725739914282
