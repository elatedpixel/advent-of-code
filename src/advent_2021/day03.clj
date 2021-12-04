(ns advent-2021.day03
  (:require [advent.core :refer [load-input]]))

(def input (load-input 2021 3))

(defn- sorted-bit-frequency [bits]
  (let [m (frequencies bits)]
    (sort-by (juxt m identity) (set bits))))

(def ^:private most-common-bit (comp last sorted-bit-frequency))
(def ^:private least-common-bit (comp first sorted-bit-frequency))

(defn gamma-rate [diagnostics]
  (Integer/parseInt (reduce str (map most-common-bit diagnostics)) 2))

(defn epsilon-rate [diagnostics]
  (Integer/parseInt (reduce str (map least-common-bit diagnostics)) 2))

(defn power-consumption [diagnostic-report]
  (let [diagnostic-by-column (apply map vector diagnostic-report)]
    (reduce * ((juxt gamma-rate epsilon-rate) diagnostic-by-column))))

;; silver
(power-consumption input)
;; => 741950

(defn- at [n] (fn [coll] (nth coll n)))

(defn- progressive-filtering-loop [bits strategy]
  (loop [i    0
         bits bits]
    (if (= 1 (count bits))
      (Integer/parseInt (first bits) 2)
      (let [bits-at-position (map (at i) bits)]
        (recur (inc i) (filter #(= (strategy bits-at-position) ((at i) %)) bits))))))

(defn- oxygen-generator-rate [coll]
  (progressive-filtering-loop coll most-common-bit))

(defn- co2-scrubber-rate [coll]
  (progressive-filtering-loop coll least-common-bit))

(defn life-support [diagnostic-report]
  (reduce * ((juxt oxygen-generator-rate co2-scrubber-rate) diagnostic-report)))

;; gold
(life-support input)
;; => 903810
