(ns advent-2023.day08 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as test]))

(def input (str/trim (slurp (io/resource "2023/day08"))))
(def sample (str/trim (slurp (io/resource "2023/day08.sample"))))
(def sample2 (str/trim (slurp (io/resource "2023/day08.sample2"))))

(defn- parse-node [s]
  (let [[_ node left right] (re-find #"(\w+) = \((\w+), (\w+)\)" s)]
    {node {\L left \R right}}))

(defn- parse [s]
  (let [[instructions network-string] (str/split s #"\n\n")
        network                       (into {} (map parse-node) (str/split-lines network-string))]
    {:instructions instructions
     :network      network}))

(defn- steps-until [stop {:keys [instructions network]}]
  (reduce
   (fn [[steps node] instruction]
          (let [node' (get-in network [node instruction])]
            (if (= stop node')
              (reduced steps)
              [(inc steps) node'])))
   [1 "AAA"]
   (cycle instructions)))

(defn- part-1 [input]
  (let [wasteland-map (parse input)]
    (steps-until "ZZZ" wasteland-map)))

(test/deftest test-part-1
  (test/is (= 2 (part-1 sample)))
  (test/is (= 6 (part-1 sample2)))
  (test/is (= 18727 (part-1 input))))

