(ns advent-2020.day18
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input "1 + 2 * 3 + 4 * 5 + 6")

(def ops
  {'+ #'+
   '* #'*})

(defn evaluate [expr]
  (walk/postwalk
   (fn [x] (if (sequential? x)
             (reduce (fn [accum [op arg]] ((ops op) accum arg))
                     (first x)
                     (partition 2 (next x)))
             x))
   expr))

(defn parse-line [line] (read-string (format "(%s)" line)))

;; part 1
(time
 (println
  (transduce (map (comp evaluate parse-line))
             +'
             (line-seq (io/reader (io/resource "2020/day18"))))))
;; 800602729153
;; "Elapsed time: 28.320694 msecs"
