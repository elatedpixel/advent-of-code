(ns advent-2023.day11
  (:require
   [clojure.test :as test]
   [advent.core :as c]
   [criterium.core :as criterium]))

(defonce sample-1
  (c/read-lines "2023/day11.sample"))

(defonce input
  (c/read-lines "2023/day11"))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn- empty-indices [input]
  (keep-indexed (fn [i v] (when (every? #{\.} v) i)) input))

(defn- galaxies [input]
  (for [[i r] (map-indexed vector input)
        [j c] (map-indexed vector r)
        :when (#{\#} c)]
    [i j]))

(defn- galaxy-map [input]
  {:gs (galaxies input)
   :er (empty-indices input)
   :ec (empty-indices (transpose input))})

(defn- countif [pred coll]
  (reduce #(if (pred %2) (inc %1) %1) 0 coll))

(defn- between? [a b c] (or (< a c b) (> a c b)))

(defn- empty-spaces [a b e]
  (countif (partial between? a b) e))

(defn- distance [t er ec [[r1 c1] [r2 c2]]]
  (+ (abs (- r1 r2))
     (abs (- c1 c2))
     (* (max 1 (dec t))
        (+ (empty-spaces r1 r2 er)
           (empty-spaces c1 c2 ec)))))

(defn- distances [{:keys [gs er ec]} t]
  (transduce
   (map (partial distance t er ec))
   +
   (for [a gs b gs :when (pos? (compare a b))] [a b])))

(defn- part-1 [input]
  (distances (galaxy-map input) 1))

(test/deftest test-part-1
  (test/is (= 374 (part-1 sample-1)))
  (test/is (= 10228230 (part-1 input))))

(defn- part-2 [input t]
  (distances (galaxy-map input) t))

;; (criterium/quick-bench (part-2 input))
;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 477.471003 ms
;;     Execution time std-deviation : 25.282487 ms
;;    Execution time lower quantile : 445.343697 ms ( 2.5%)
;;    Execution time upper quantile : 500.168374 ms (97.5%)
;;                    Overhead used : 15.952927 ns

(test/deftest test-part-2
  (test/are [want g t] (= want (part-2 g t))
    1030 sample-1 10
    8410 sample-1 100
    447073334102 input 1000000))
