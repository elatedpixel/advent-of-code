(ns advent-2023.day18
  (:require
   [advent.core :as c]
   [clojure.string :as str]
   [clojure.test :as test]))

(def dir
  {:D [1 0]
   :L [0 -1]
   :R [0 1]
   :U [-1 0]})

(defrecord DigStep [direction distance hex-color])
(defn- dig-step [^String dig-plan-row]
  (let [[_ direction distance hex-color] (re-find #"([DLRU]) (\d+) \(([#0-9a-f]+)\)" dig-plan-row)]
    (->DigStep (dir (keyword (str direction)))
               (parse-long distance)
               hex-color)))

(def sample
  (c/read-lines (c/file 2023 18 ".sample")
                (map dig-step)))

(def input
  (c/read-lines (c/file 2023 18)
                (map dig-step)))

(defn- trench-size [dig-site row]
  (let [[x X] (apply (juxt min max) (keys (dig-site row)))]
    ((reduce
      (fn [[sum trench? prev] x]
        (let [current    (get-in dig-site [row x])
              in-trench? (if (and (some? prev) (nil? current))
                           (not trench?)
                           trench?)]
          [(if (or in-trench? (some? current)) (inc sum) sum) in-trench? current]))
      [0 false nil]
      (range x (inc X)))
     0)))

(defn- area [dig-site]
  (let [[y Y] (apply (juxt min max) (keys dig-site))]
    (transduce
     (map (partial trench-size dig-site))
     +
     0
     (range y (inc Y)))))

(defn- dig-site [dig-plans]
  (:dig-site
   (reduce
    (fn [{:keys [dig-site position] :as state} {:keys [direction distance hex-color]}]
      (-> state
          (assoc :position (mapv + position (mapv (partial * distance) direction)))
          (assoc :dig-site (reduce
                            (fn [dig-site [y x]] (assoc-in dig-site [y x] hex-color))
                            dig-site
                            (map #(mapv + position (mapv (partial * %) direction)) (range distance))))))
    {:dig-site {}
     :position [0 0]}
    dig-plans)))

(defn- draw [dig-site]
  (let [rows (inc (apply max (keys dig-site)))
        cols (inc (apply max (mapcat keys (vals dig-site))))]
    (println
     (str/join
      \newline
      (for [i (range rows)]
        (apply str (for [j (range cols)] (str (first (get-in dig-site [i j] ".")))))))
     \newline)))

(defn- tap-> [any f]
  (f any)
  any)

(defn- part-1 [dig-plan]
  (-> dig-plan
      dig-site
      ;; (tap-> draw)
      area))

(test/deftest test-part-1
  (test/is (= 62 (part-1 sample))))

;; (part-1 input)
;; => 65309
