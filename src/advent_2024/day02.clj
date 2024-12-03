(ns advent-2024.day02
  (:require
   [clojure.java.io :as io]))

(defn- deltas [report]
  (mapv (fn [[a b]] (- a b)) (partition 2 1 report)))

(defn- safe?
  "A report is safe iff: the reports are only increasing or decreasing, the delta between levels is [1, 3]"
  [report]
  (let [d (deltas report)]
    (and (or (every? pos? d)
             (every? neg? d))
         (every? (fn [n] (<= 1 (if (neg? n) (* n -1) n) 3)) d))))

(defn- without-index [v]
  (fn [i] (into (subvec v 0 i)
                (subvec v (inc i)))))

(defn- mostly-safe?
  "A report is mostly safe iff it's safe? or if removing one element would make it safe."
  [report]
  (some safe?
        (cons report
              (mapv (without-index report)
                    (range (count report))))))

(defn -main []
  (let [input   (line-seq (io/reader (io/resource "2024/day02.txt")))
        reports (map (fn [s] (read-string (format "[%s]" s))) input)]
    (println (count (filter safe? reports)))
    (println (count (filter (comp true? mostly-safe?) reports)))))
