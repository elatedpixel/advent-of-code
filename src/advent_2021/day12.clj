(ns advent-2021.day12
  (:require [advent.core :as c]))

(def input (c/load-input 2021 12))

(defn adj-list [coll]
  (reduce (fn [m [a b]] (-> m
                            (update a conj b)
                            (update b conj a)))
          {}
          (map #(clojure.string/split % #"-") coll)))

(def caves (adj-list input))

(defn big-cave? [s] (boolean (re-matches #"[A-Z]+" s)))

(defn search [caves start end]
  ((fn all-paths [frontier explored]
     (let [[cave frontier] ((juxt peek pop) frontier)
           explored        (if (big-cave? cave) explored (conj explored cave))]
       (if (= end cave)
         '((cave))
         (mapcat (fn [next-cave]
                   (map (fn [path] (cons cave path))
                        (all-paths (conj frontier next-cave) explored)))
                 (remove explored (caves cave))))))
   [start] #{}))

(count (search caves "start" "end"))
;; => 4011

(defn search-again [caves start end]
  (letfn [(paths [cave visited doubled?]
              (if (= cave end)
                '((cave))
                (let [did-double     (or doubled? (boolean (visited cave)))
                      to-visit       (remove #{start}  ; May never return to start
                                             (if did-double
                                               (remove visited (caves cave))
                                               (caves cave)))
                      visited        (if (big-cave? cave)
                                       visited
                                       (conj visited cave))
                      visit-neighbor (fn [neighbor]
                                       (map (fn [path] (cons cave path))
                                            (paths neighbor
                                                   visited
                                                   did-double)))]
                  (mapcat visit-neighbor to-visit))))]
      (paths start #{} false)))

(time (count (search-again caves "start" "end")))
;; => 108035
