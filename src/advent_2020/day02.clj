(ns advent-2020.day02
  (:require [clojure.java.io :as io]))

(defn- parse [s]
  (rest (re-matches #"(\d+)-(\d+) ([a-z]): (\w+)" s)))

(def ^:private input
  (sequence
   (map parse)
   (line-seq (io/reader (io/resource "2020/day02")))))

(defn count-valid-passwords [test-fn passwords]
  (time (println (count (filter test-fn passwords)))))

(comment
                                        ;part 1
  (letfn [(valid-password? [[from to letter password]]
            "a valid `password` has [`from`-`to`] occurrences of `letter`"
            (<= (Integer/parseInt from)
                (get (frequencies password) (first letter) 0)
                (Integer/parseInt to)))]
    (count-valid-passwords valid-password? input))
  ;; 660
  ;; "Elapsed time: 10.796568 msecs"

                                        ;part 2
  (letfn [(valid-password? [[index-a index-b letter password]]
            "a valid `password` has `letter` at `index-a` xor `index-b`"
            (let [a (get password (dec (Integer/parseInt index-a)))
                  b (get password (dec (Integer/parseInt index-b)))
                  c (first letter)]
              (and (or (= a c) (= b c))
                   (not= a b))))]
    (count-valid-passwords valid-password? input))
  ;; 530
  ;; "Elapsed time: 4.24681 msecs"
                                        ;
  )
