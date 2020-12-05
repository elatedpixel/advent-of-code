(ns advent-2020.day04
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.string :as s]))

(defn- parse-field [s]
  (let [[k v] (s/split s #":")]
    [(keyword k) v]))

(defn- parse-fields [s]
  (->> (s/split s #"\s+")
       (map parse-field)
       (into {})))

(def input
  (map parse-fields (s/split (slurp (io/resource "2020/day04")) #"\n\n")))

(spec/def ::passport-part1
  (spec/keys :req-un [::ecl ::byr ::iyr ::hgt ::pid ::hcl ::eyr]
             :opt [::cid]))

(comment
  ;; part 1
  (count (filter #(spec/valid? ::passport-part1 %) input)))

