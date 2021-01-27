(ns advent-2015.day05
  (:require [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(def input (line-seq (io/reader (io/resource "2015/day05"))))

(s/def ::at-least-three-vowels
  #(>= (count (re-seq #"[aeiou]" %)) 3))

(s/def ::double-letter
  #(re-find #"(.)\1" %))

(s/def ::does-not-contain-strings
  #(not (re-find #"(ab|cd|pq|xy)" %)))

(s/def ::nice-string
  (s/and ::at-least-three-vowels
         ::double-letter
         ::does-not-contain-strings))

;; examples
(assert (s/valid? ::nice-string "ugknbfddgicrmopn"))
(assert (s/valid? ::nice-string "aaa"))
(assert (not (s/valid? ::nice-string "jchzalrnumimnmhp")))
(assert (not (s/valid? ::nice-string "haegwjzuvuyypxyu")))
(assert (not (s/valid? ::nice-string "dvszwmarrgswjxmb")))

(comment
  ;; part 1
  (time (println (count (filter #(s/valid? ::nice-string %) input)))))

(s/def ::two-letters-that-appear-twice
  #(re-find #"([a-z]{2}).*\1" %))

(s/def ::character-repeats-with-separator
  #(re-find #"([a-z]).\1" %))

(s/def ::nice-string-again
  (s/and ::two-letters-that-appear-twice
         ::character-repeats-with-separator))

;; examples
(assert (s/valid? ::nice-string-again "qjhvhtzxzqqjkmpb"))
(assert (s/valid? ::nice-string-again "xxyxx"))
(assert (not (s/valid? ::nice-string-again "uurcxstgmygtbstg")))
(assert (not (s/valid? ::nice-string-again "ieodomkazucvgmuy")))

(comment
  ;; part 2
  (time (println (count (filter #(s/valid? ::nice-string-again %) input)))))
