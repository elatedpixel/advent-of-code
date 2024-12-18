(ns advent-2024.day12
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(defn- parse [file]
  (mapv vec (line-seq (io/reader (io/resource file)))))

(def ^:private puzzle (parse "2024/day12.txt"))
(def ^:private sample0 (parse "2024/day12.sample0"))
(def ^:private sample1 (parse "2024/day12.sample1"))
(def ^:private sample2 (parse "2024/day12.sample2"))

(def offsets [[-1 0] [1 0] [0 -1] [0 1]])

(defn- graph-value ^Character [graph [x y]]
  (get-in graph [y x] \space))

(defn- dfs
  [graph explored [x y :as node]]
  (if (@explored node)
    [0 0 0]
    (do (swap! explored conj node)
        (reduce
          (fn [v1 v2] (mapv + v1 v2))
          [1 0 0]
          (let [plant (graph-value graph node)]
            (for [[dx dy :as offset] offsets
                  :let               [p (mapv + node offset)]]
              (if (= plant (graph-value graph p))
                (dfs graph explored p)
                [0 1 (if (not (and (= plant (graph-value graph [(+ x dy) (- y dx)]))
                                   (not= plant (graph-value graph [(+ x dx dy) (- (+ y dy) dx)]))))
                       0 1)])))))))

(defn part-1 [input]
  (let [explored (atom #{})
        regions (for [y (range (count input))
                      x (range (count (input y)))]
                  (dfs input explored [x y]))]
    (reduce +' (map (fn [[a p _]] (* a p)) regions))))

(t/deftest test-part-1
  (t/is (= 140 (part-1 sample0)))
  (t/is (= 772 (part-1 sample1)))
  (t/is (= 1930 (part-1 sample2))))

(defn part-2 [input]
  (let [explored (atom #{})
        regions  (for [y (range (count input))
                       x (range (count (input y)))]
                  (dfs input explored [x y]))]
    (reduce +' (map (fn [[a _ p]] (* a p)) regions))))

(t/deftest test-part-2
  (t/is (= 80 (part-2 sample0)))
  (t/is (= 1206 (part-2 sample2))))

(defn -main []
  (println (str "Day 12 Part 1: " (part-1 puzzle)))
  (println (str "Day 12 Part 2: " (part-2 puzzle))))
