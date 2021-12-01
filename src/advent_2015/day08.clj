(ns advent-2015.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [replace]]))

(def input (line-seq (io/reader (io/resource "2015/day08"))))

(defn memory-length [string]
  (count
   (replace (subs string 1 (dec (count string)))
            #"(\\x[0-9a-f]{2})|(\\\")|(\\\\)"
            "_")))

(defn difference-reducer [total [literal memory]]
  (+ total (- literal memory)))

(defn delta-lengths [delta strings]
  (transduce (map delta)
             (completing difference-reducer)
             0
             strings))

(comment
  ;; part 1
  (delta-lengths (juxt count memory-length) input)
  ;; => 1371
  )

(defn encoded-length [string]
  (count (with-out-str (pr string))))

(comment
  ;; part 2
  (delta-lengths (juxt encoded-length count) input)
  ;; => 2117
  )
