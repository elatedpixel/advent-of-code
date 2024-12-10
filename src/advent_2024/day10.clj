(ns advent-2024.day10
  (:require
   [clojure.java.io :as io]
   [clojure.test :as t]))

(def ^:private sample0
  (line-seq (io/reader (io/resource "2024/day10.sample0"))))

(def ^:private puzzle
  (line-seq (io/reader (io/resource "2024/day10.txt"))))

(defn- parse
  "Build data structures for puzzle: list of trailheads and topographical map."
  [input]
  (reduce-kv
    (fn [m y row]
      (reduce-kv
        (fn [m x height-string]
          (let [height (Character/getNumericValue height-string)]
            (cond-> m
              (zero? height) (update :trailheads conj [x y])
              :always        (assoc-in [:topography [x y]] height))))
        m
        (vec row)))
    {:trailheads []
     :topography {}}
   (vec input)))

(defn- trails
  "Returns map[trailhead][]trails."
  [{:keys [trailheads topography]}]
  (letfn
      [(good-hiking [node]
         (let [height (get topography node)]
           (filter (fn [coord] (= 1 (- (get topography coord height) height)))
                   (map #(mapv + node %) '([-1 0] [1 0] [0 1] [0 -1])))))
       (trails [node explored]
         (cond (explored node)         '(())
               (= 9 (topography node)) [[node]]
               :else                   (mapcat
                                         (fn [next-coord]
                                           (map (fn [trail] (cons node trail))
                                                (trails next-coord (conj explored node))))
                                         (remove explored (good-hiking node)))))]
    (into {}
          (for [trailhead trailheads]
               [trailhead (trails trailhead #{})])))
  )

(defn- trailhead-scores
  [trails]
  (reduce
    +
    (map (fn [[k v]] (count (distinct (map last v))))
         trails)))

(defn- trailhead-ratings
  [trails]
  (reduce
    +
    (map (fn [[k v]] (count (distinct v)))
         trails)))

(comment
  ((trails (parse sample0)) [2 0])
  ;; => (([2 0] [3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [0 2] [0 3])
  ;;     ([2 0] [3 0] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [1 1] [1 0])
  ;;     ([2 0] [3 0] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [5 4])
  ;;     ([2 0] [3 0] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [4 5])
  ;;     ([2 0] [3 0] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [4 3])
  ;;     ([2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [0 2] [0 3])
  ;;     ([2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [1 1] [1 0])
  ;;     ([2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [5 4])
  ;;     ([2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [4 5])
  ;;     ([2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [4 3])
  ;;     ([2 0] [2 1] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [0 2] [0 3])
  ;;     ([2 0] [2 1] [3 1] [3 2] [2 2] [2 3] [1 3] [1 2] [1 1] [1 0])
  ;;     ([2 0] [2 1] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [5 4])
  ;;     ([2 0] [2 1] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [4 5])
  ;;     ([2 0] [2 1] [3 1] [3 2] [2 2] [2 3] [2 4] [3 4] [4 4] [4 3])
  ;;     ([2 0] [2 1] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [0 2] [0 3])
  ;;     ([2 0] [2 1] [3 1] [3 2] [3 3] [2 3] [1 3] [1 2] [1 1] [1 0])
  ;;     ([2 0] [2 1] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [5 4])
  ;;     ([2 0] [2 1] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [4 5])
  ;;     ([2 0] [2 1] [3 1] [3 2] [3 3] [2 3] [2 4] [3 4] [4 4] [4 3]))

  (distinct (map last ((trails (parse sample0)) [2 0])))
  ;; => ([0 3] [1 0] [5 4] [4 5] [4 3])
  )

(defn part-1 [input]
  (trailhead-scores (trails (parse input))))

(t/deftest test-part-1
  (t/is (= 36 (part-1 sample0))))

(defn part-2 [input]
  (trailhead-ratings (trails (parse input))))

(t/deftest test-part-2
  (t/is (= 81 (part-1 sample0))))

(defn -main []
  (println (str "Day 10 Part 1: " (part-1 puzzle)))
  (println (str "Day 10 Part 2: " (part-2 puzzle))))
