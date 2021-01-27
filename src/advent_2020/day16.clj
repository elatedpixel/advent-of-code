(ns advent-2020.day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(defn inclusive-range? [[a b]]
  (fn [n] (<= a n b)))

(defn parse-rule [rule]
  (->> rule
       (re-seq #"\d+")
       (map read-string)
       (partition 2)
       (map inclusive-range?)
       (apply some-fn)))

(defn parse-ticket [ticket]
  (read-string (format "(%s)" ticket)))

(defn parse-input [input]
  (-> input
      (str/split #"\n\n")
      ((fn [[rules your-ticket nearby-tickets]]
         {:rules          (into [] (map parse-rule) (str/split-lines rules))
          :your-ticket    (parse-ticket (second (str/split-lines your-ticket)))
          :nearby-tickets (into [] (map parse-ticket) (rest (str/split-lines nearby-tickets)))}))))

(defn ticket-scanning-error-rate [rules tickets]
  (map (fn [ticket] (filter (complement (apply some-fn rules)) ticket)) tickets))

(def input (slurp (io/resource "2020/day16")))

(let [notes (parse-input input)]
  (->> (ticket-scanning-error-rate (:rules notes) (:nearby-tickets notes))
       (map #(reduce + %))
       (reduce +)))
;; => 23044

(let [notes (parse-input input)]
  ((comp clojure.pprint/pprint :nearby-tickets)
   (assoc notes :nearby-tickets (filter #(every? (apply some-fn (:rules notes)) %)
                                        (:nearby-tickets notes)))))
