(ns advent-2023.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day05"))))
(def sample (str/trim (slurp (io/resource "2023/day05.sample"))))

(defn- to-ints
  [s]
  (mapv read-string (re-seq #"\d+" s)))

(defn- range* [[dst src len]]
  (into {} (map vector
                (range src (+ src len))
                (range dst (+ dst len)))))

(defn- lookup [r]
  (fn [x] (get r x x)))

(defn- resolve* [m]
  (comp
   (lookup (:humidity-to-location m))
   (lookup (:temperature-to-humidity m))
   (lookup (:light-to-temperature m))
   (lookup (:water-to-light m))
   (lookup (:fertilizer-to-water m))
   (lookup (:soil-to-fertilizer m))
   (lookup (:seed-to-soil m))))
;; Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.

(defn- parse [input]
  (let [sections (str/split input #"\n\n")]
    (reduce
     (fn [m s]
       (cond
         (.startsWith s "seeds")
         (assoc m :seeds (to-ints s))

         :else
         (let [[name-str & ranges-str] (str/split-lines s)
               name                    (keyword (re-find #"[\w-]+" name-str))
               ranges                  (transduce (map (comp range* to-ints)) merge ranges-str)]
           (assoc m name ranges))))
     {}
     sections)))
(comment
  (parse sample))

(defn part-1 [input]
  (let [almanac (parse input)]
    (apply min (map (resolve* almanac) (:seeds almanac)))))

(part-1 input)
;; => OutOfMemoryError Java heap space  clojure.lang.PersistentHashMap$BitmapIndexedNode.assoc (PersistentHashMap.java:882)

(test/deftest part1
  (test/is (= 35 (part-1 sample))))
