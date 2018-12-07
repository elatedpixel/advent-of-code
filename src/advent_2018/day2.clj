(ns advent-2018.day2
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

(t/with-test

  (defn diff [s1 s2]
    (loop [pairs  (map vector s1 s2)
           same []
           d 0]
      (if (empty? pairs) [d (apply str same)]
          (let [[a b] (first pairs)]
            (recur (rest pairs)
                   (if (= a b) (conj same a) same)
                   (+ d (if (= a b) 0 1)))))))

  (t/is (= [1 "fgij"] (diff "fghij" "fguij")))
  (t/is (= [4 "y"] (diff "axcye" "wvxyz"))))

(t/with-test

  (defn checksum-common [coll]
    (loop [[[error match] & xs]
           (for [[i a] (keep-indexed vector coll)
                      [j b] (keep-indexed vector coll)
                      :when (not= i j)]
                  (diff a b))]
      (if (= 1 error) match
          (recur xs))))

  (t/is (= "fgij" (checksum-common ["abcde"
                                    "fghij"
                                    "klmno"
                                    "pqrst"
                                    "fguij"
                                    "axcye"
                                    "wvxyz"]))))

(t/run-tests 'day2.core)

(defn -main [input-file]
  (let [input (clojure.string/split-lines (slurp input-file))]
    (prn)
    (println (str "Day 2 Part 1: " (time (checksum input))))
    (println (str "Day 2 Part 2: " (time (checksum-common input))))))
