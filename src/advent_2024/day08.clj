(ns advent-2024.day08
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(defn- parse
  [file]
  (->> file
       (io/resource)
       (io/reader)
       (line-seq)
       (map-indexed vector)
       (reduce
         (fn [m [y line]]
           (reduce
             (fn [m [x c]]
               (cond-> m
                 (not= c \.) (update-in [:antenna c] (fnil conj []) [x y])
                 :always     (assoc-in [:grid [x y]] c)))
             m
             (map-indexed vector line)))
         {})))

(def ^:private sample0
  (parse "2024/day08.sample0"))

(defn part-1 [puzzle] 0)

(t/deftest test-part-1
  (t/is (= 14 (part-1 sample0))))
