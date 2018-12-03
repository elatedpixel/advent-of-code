(ns day2.core
  (:require [clojure.test :as t]))

(t/with-test

  (defn checksum [coll]
    (let [coll-f (map frequencies coll)
          coll-2 (count (filter #(some #{2} (vals %)) coll-f))
          coll-3 (count (filter #(some #{3} (vals %)) coll-f))]
      (* coll-2 coll-3)))

  (t/is (= 12 (checksum ["abcdef"
                         "bababc"
                         "abbcde"
                         "abcccd"
                         "aabcdd"
                         "abcdee"
                         "ababab"]))))

(t/run-tests 'day2.core)

(defn -main [input-file]
  (let [input (clojure.string/split-lines (slurp input-file))]
    (prn)
    (println (str "Day 2 Part 1: " (time (checksum input))))))
