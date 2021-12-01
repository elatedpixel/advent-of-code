(ns advent-2015.day12
  (:require [advent.core :refer (load-input)]
            [cheshire.core :refer (parse-string)]
            [clojure.walk :refer (postwalk)]))

(def input (load-input 2015 12))

;; part 1
(transduce (map #(Integer/parseInt %))
           +
           (re-seq #"[\d-]+" (first input)))
;; => 119433

(def json (parse-string (first input)))

;; part 2
(def not-red
  (postwalk
    #(when (not (and (map? %) (some #{"red"} (vals %)))) %)
    json))

(let [sum (atom 0)]
  (postwalk (fn [x] (if (number? x) (swap! sum + x)) x) not-red)
  (println @sum))
