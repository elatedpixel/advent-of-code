(ns advent-2024.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.test :as t]))

(def ^:private puzzle (slurp (io/resource "2024/day05.txt")))
(def ^:private sample0 (slurp (io/resource "2024/day05.sample0")))

(defn- parse
  "Returns a vector of [rules, updates] where rules (X|Y) is of the form map[Y][]Xs and updates are []long."
  [input]
  (let [[rules-list updates-list] (s/split input #"\n\n")
        rules (reduce (fn [m rule]
                        (let [[_ a b] (re-find #"(\d+)\|(\d+)" rule)]
                          (update m (parse-long b) (fnil conj []) (parse-long a))))
                      {}
                      (s/split-lines rules-list))
        updates (map (fn [updates]
                       (mapv
                        parse-long
                        (s/split updates #",")))
                     (s/split-lines updates-list))]
    [rules updates]))

(defn- ordered?
  "Predicate that keeps track of page numbers to guarantee print ordering."
  [rules]
  (fn [update]
    (let [all-pages (set update)]
      (loop [[page & pages] update
             seen           #{}]
        (cond
          (nil? page)
          true

          (and (some? (rules page))
               (not-every? (fn [other-page] (or (some? (seen other-page))
                                                (nil? (all-pages other-page))))
                           (rules page)))
          false

          :else
          (recur pages (conj seen page)))))))

(defn part-1
  "Filter out updates that are not ordered correctly, find the middle value from each, return sum."
  [input]
  (let [[rules updates] (parse input)]
    (reduce
     +
     (eduction
      (filter (ordered? rules))
      #dbg (map (fn [update] (get update (long (/ (count update) 2)))))
      updates))))

(t/deftest test-part-1
  (t/is (= 143 (part-1 sample0))))

(defn -main []
  (println (str "Day 5 Part 1: " (part-1 puzzle))))
