(ns advent-2020.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn cycles [n]
  (reductions + (cycle (conj (vec (repeat n 0)) n))))

(defn next-bus [timestamp ids]
  (apply min-key second
         (map
          (juxt identity
                (fn [id] (some #(when (>= % timestamp) %) (cycles id))))
          ids)))

(def sample-input "939
7,13,x,x,59,x,31,19")

(defn parse-input [s]
  (let [[timestamp ids-string] (str/split s #"\n")
        ids                    (str/split ids-string #"[x,]+")]
    (mapv #(Integer/parseInt %) (cons timestamp ids))))

(def input (slurp (io/resource "2020/day13")))

(comment
  (time
   (let [[timestamp & ids]  (parse-input input)
         [id bus-timestamp] (next-bus timestamp ids)]
     (println
      (* id (- bus-timestamp timestamp))))))

(defn parse-input-again [s]
  (let [[timestamp ids-string] (str/split s #"\n")
        ids                    (str/split ids-string #",")]
    (keep-indexed (fn [i x] (when (not= "x" x) [i (Integer/parseInt x)])) ids)))

(comment
  (time
   (let [[[index id] & more] (parse-input-again input)]
     (loop [timestamp (- id index)
            step      id
            more      more]
       (if-let [[index id] (first more)]
         (if (zero? (mod (+ timestamp index) id))
           (recur timestamp (* step id) (rest more))
           (recur (+ timestamp step) step more))
         timestamp)
       ))))
