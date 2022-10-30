(ns advent-2015.day14
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]))

(def input (line-seq (io/reader (io/resource "2015/day14"))))

(def regex (re-pattern #"(?<name>\w+) can fly (?<kms>\d+) km/s for (?<duration>\d+) seconds, but then must rest for (?<sleep>\d+) seconds."))

(defn fly [kms duration sleep start]
  (lazy-cat
   (take duration (iterate (partial + kms) start))
   (repeat sleep (+ start (* kms duration)))
   (fly kms duration sleep (+ start (* kms duration)))))

(defn flying-reindeer [[title kms duration sleep]]
  (let [flight (drop 1 (fly (Integer/parseInt kms)
                            (Integer/parseInt duration)
                            (Integer/parseInt sleep)
                            0))]
    {:title  title
     :flight flight
     :score  0}))

(def data (into {} (comp (map #(flying-reindeer (rest (re-matches regex %))))
                      (map (juxt :title identity)))
                input))

;; silver
(apply max (map #(nth (:flight %) 2503) (vals data)))
;; => 2640

;; gold
(defn- update-score
  [data ks]
  (reduce
   (fn [m k] (update-in m [k :score] inc)) data ks))

(defn- filter-keys-by
  [data pred]
  (reduce (fn [a [k v]] (if (pred v) (conj a k) a)) [] data))

(defn- winner
  [data]
  (let [winner (apply max-key (fn [[k v]] (:score v)) data)]
        (dissoc (second winner) :flight)))

(defn- keys-with-max-flight
  [data step]
  (let [mx (apply max (map #(nth (:flight %) step) (vals data)))]
    (filter-keys-by data #(= mx (nth (:flight %) step)))))

(defn gold [n]
  (loop [step 0
         data data]
    (if (= step n)
      (winner data)
      (recur (inc step)
             (update-score data (keys-with-max-flight data step))))))

(gold 2504)
