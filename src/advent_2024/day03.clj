(ns advent-2024.day03
  (:require [clojure.java.io :as io]))

(defn- read-file [file]
  (slurp (io/resource file)))

(defn- multiply
  "Parse and multiply regex match vector like [whole-string, int-string, int-string]."
  [match]
  (let [[_ a b] match]
    (if
      (or (nil? a) (nil? b)) 0
      (* (parse-long a) (parse-long b)))))

;; part 1, sum all mul() operations.
(defn- multiply-all [line]
  (transduce
   (map multiply)
   +
   0
   (re-seq #"mul\((\d+),(\d+)\)" line)))

;; stateful transducer for part 2, filters mul() operations when disabled
(defn- exclude-disabled []
  (fn [rf]
    (let [enabled (volatile! true)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [on @enabled]
           (vreset! enabled (case (first input)
                              "do()" true
                              "don't()" false
                              on))
           (if on (rf result input) result)))))))

;; part 2, sum all mul() operations enabled by do().
(defn- multiply-enabled [line]
  (transduce
   (comp
    (exclude-disabled)
    (map multiply))
   +
   0
   (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" line)))


(defn -main []
  (let [input   (read-file "2024/day03.txt")
        sample0 (read-file "2024/day03.sample0")
        sample1 (read-file "2024/day03.sample1")]
    (println (multiply-all input))
    (println (multiply-enabled input))))
