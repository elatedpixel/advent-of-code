(ns advent-2020.day02
  (:require [clojure.java.io :as io]))

(defn valid-password? [test-fn s]
  (let [regex #"(\d+)-(\d+) ([a-z]): (\w+)"]
    (test-fn (rest (re-matches regex s)))))

(def input
  (line-seq (io/reader (io/resource "2020/day02"))))

(comment
                                        ;part 1
  (letfn [(test-password [[from to letter password]]
            (<= (Integer/parseInt from)
                (get (frequencies password) (first letter) 0)
                (Integer/parseInt to)))]
    (time (println (count (filter (partial valid-password? test-password) input)))))
  ;; 660
  ;; "Elapsed time: 10.796568 msecs"
                                        ;
  )

(comment
                                        ;part 2
  (letfn [(test-password [[index-a index-b letter password]]
            (let [a (get password (dec (Integer/parseInt index-a)))
                  b (get password (dec (Integer/parseInt index-b)))
                  c (first letter)]
              (and (or (= a c) (= b c))
                   (not= a b))))]
    (time (println (count (filter (partial valid-password? test-password) input)))))
  ;; 530
  ;; "Elapsed time: 4.24681 msecs"
                                        ;
  )
