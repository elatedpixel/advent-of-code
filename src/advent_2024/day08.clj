(ns advent-2024.day08
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(defn- parse
  [file]
  (->> file
       (io/resource)
       (io/reader)
       (line-seq)
       vec
       (reduce-kv
         (fn [m y line]
           (reduce-kv
             (fn [m x c]
               (cond-> m
                 (not= c \.) (update-in [:antenna c] (fnil conj []) [x y])
                 :always     (assoc-in [:grid [x y]] c)))
             m
             (vec line)))
         {})))

(def ^:private sample0 (parse "2024/day08.sample0"))
(def ^:private puzzle (parse "2024/day08.txt"))

(defn- project-slope
  ([slope]
   (fn [coord]
     (mapv + slope coord)))
  ([a b]
   (let [slope (mapv - b a)]
     (mapv + slope b)))
  ([grid a b]
   (let [slope (mapv - b a)]
     (take-while (fn [coord] (some? (grid coord)))
                 (iterate (project-slope slope) b)))))

(defn- antinodes
  [{:keys [grid antenna] :as puzzle}]
  (reduce-kv
    (fn [m antenna-char antenna-coords]
      (into
        m
        (filter (fn [coord] (some? (grid coord))))
        (for [a     antenna-coords
              b     antenna-coords
              :when (not= a b)]
          (project-slope a b))))
    #{}
    antenna))

(defn- resonant-frequency-antinodes
  [{:keys [grid antenna] :as puzzle}]
  (reduce-kv
    (fn [m antenna-char antenna-coords]
      (into
        m
        (mapcat concat)
        (for [a     antenna-coords
              b     antenna-coords
              :when (not= a b)]
          (project-slope grid a b))))
    #{}
    antenna))

(comment
  (antinodes sample0)
  ;; => #{[0 7]
  ;;      [1 5]
  ;;      [2 3]
  ;;      [3 1]
  ;;      [3 6]
  ;;      [4 2]
  ;;      [6 0]
  ;;      [6 5]
  ;;      [7 7]
  ;;      [9 4]
  ;;      [10 2]
  ;;      [10 10]
  ;;      [10 11]
  ;;      [11 0]}

  (resonant-frequency-antinodes sample0)
  ;; => #{[0 0]
  ;;      [0 7]
  ;;      [1 0]
  ;;      [1 1]
  ;;      [1 5]
  ;;      [1 10]
  ;;      [2 2]
  ;;      [2 3]
  ;;      [2 8]
  ;;      [3 1]
  ;;      [3 3]
  ;;      [3 6]
  ;;      [3 11]
  ;;      [4 2]
  ;;      [4 4]
  ;;      [4 9]
  ;;      [5 2]
  ;;      [5 5]
  ;;      [5 7]
  ;;      [6 0]
  ;;      [6 5]
  ;;      [6 6]
  ;;      [7 3]
  ;;      [7 7]
  ;;      [8 1]
  ;;      [8 8]
  ;;      [9 4]
  ;;      [9 9]
  ;;      [10 2]
  ;;      [10 10]
  ;;      [10 11]
  ;;      [11 0]
  ;;      [11 5]
  ;;      [11 11]}
  )

(defn part-1 [puzzle]
  (count (antinodes puzzle)))

(t/deftest test-part-1
  (t/is (= 14 (part-1 sample0))))

(defn part-2 [puzzle]
  (count (resonant-frequency-antinodes puzzle)))

(t/deftest test-part-2
  (t/is (= 34 (part-2 sample0))))

(defn -main []
  (println (str "Day 8 Part 1: " (part-1 puzzle)))
  (println (str "Day 8 Part 2: " (part-2 puzzle))))
