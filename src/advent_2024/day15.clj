(ns advent-2024.day15
  (:require
   [clojure.test :as t]
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defrecord Warehouse [robot warehouse])
(defrecord Puzzle [^Warehouse warehouse movements])

(defn- parse [input]
  (let [[warehouse movements] (s/split input #"\n\n")
        warehouse-rows        (s/split-lines warehouse)]
    (Puzzle.
     (Warehouse.
      (first
       (for  [y     (range (count warehouse-rows))
              x     (range (count (warehouse-rows 0)))
              :when (= \@ (nth (nth warehouse-rows x) y))]
         [y x]))
      (into [] (map vec) warehouse-rows))
     (into [] (filter (set "^v<>")) movements))))

(def ^:private puzzle
  (parse (slurp (io/resource "2024/day15.txt"))))
(def ^:private sample0
  (parse (slurp (io/resource "2024/day15.sample0"))))

(def ^:private offset
  {\^ [-1 0]
   \v [1 0]
   \< [0 -1]
   \> [0 1]})

(defn- vec+
  ([v v'] (mapv + v v'))
  ([v v' & more]
   (reduce vec+ v (cons v' more))))

(t/deftest test-vec+
  (t/is (= [1 4] (vec+ [1 2] [0 1] [0 1]))))

(defn- display [^Warehouse {:keys [warehouse]}]
  (doseq [row warehouse]
    (println (reduce str row)))
  (println))

(defn- push
  "`rotate` values: `@###.` should be `.@###` and `@.` should be `.@`."
  [{:keys [robot warehouse] :as puzzle} coords]
  (let [[[y x] & more] coords]
    (reduce
     (fn [puzzle [y x]]
       (assoc-in puzzle [:warehouse y x] \O))
     (-> puzzle
         (assoc :robot [y x])
         (assoc-in [:warehouse (robot 0) (robot 1)] \.)
         (assoc-in [:warehouse y x] \@))
     more)))

(defn- try-move [{:keys [robot warehouse] :as puzzle} movement]
  (let [direction  (offset movement)
        target     (vec+ robot direction)
        target-sym (get-in warehouse target)]
    (case target-sym
      \# puzzle
      \. (push puzzle [target])
      \O (let [crates           (into []
                                      (take-while #(= \O (get-in warehouse %)))
                                      (iterate (partial vec+ direction) target))
               crate-target     (vec+ direction (last crates))
               crate-target-sym (get-in warehouse crate-target)]
           (case crate-target-sym
             \# puzzle
             \. (push puzzle (conj crates crate-target))
             (throw (Exception. (format "Unexpected crate-target: %s at %s." crate-target-sym crate-target))))))))

(defn- part-1 [^Puzzle {:keys [warehouse movements]}]
  (reduce-kv
   (fn [sum y row]
     (reduce-kv
      (fn [sum x sym]
        (if (= \O sym) (+ sum (* 100 y) x)
            sum))
      sum
      row))
   0
   (:warehouse (reduce try-move warehouse movements))))

(defn -main []
  (println (format "Day 15 Part 1: %d" (part-1 puzzle))))
