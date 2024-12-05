(ns advent-2024.day02
  (:require
   [clojure.java.io :as io]))

(defn- safe?
  "A report is safe iff: the reports are only increasing or decreasing, the delta between levels is [1, 3]."
  [report]
  (and (or (every? true? (map > report (rest report)))
           (every? true? (map < report (rest report))))
       (every? #(<= 1 (abs %) 3) (map - report (rest report)))))

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
