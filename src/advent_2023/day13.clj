(ns advent-2023.day13
  (:require
   [advent.core :as c]
   [clojure.string :as str]
   [clojure.test :as test]))

;; point of incidence

(def input (c/read-paragraphs (c/file 2023 13)))
(def sample (c/read-paragraphs (c/file 2023 13 ".sample")))

(defn- parse [notes]
  (map str/split-lines notes))

(defn- mirror? [eq m i]
  (let [[l r] (split-at i m)
        n (min (count l) (count r))]
    (when (< i (count m))
      (if (and (pos? n) (eq (take n (reverse l)) (take n r)))
       (count l)
       (recur eq m (inc i))))))

(defn- score-mirror [eq note]
  (or (mirror? eq (apply map str note) 0)
      (* 100 (mirror? eq note 0))))

(defn- part-1 [input]
  (transduce (map (partial score-mirror #'=)) + (parse input)))

(test/deftest test-part-1
  (test/is (= 405 (part-1 sample)))
  (test/is (= 31739 (part-1 input))))

(defn- edit-distance-1 [a b]
  (= 1 (reduce + (mapcat (partial map #(if (= %1 %2) 0 1)) a b))))

(defn- part-2 [input]
  (transduce (map (partial score-mirror #'edit-distance-1)) + (parse input)))

;; (part-2 input)
;; => 31539
