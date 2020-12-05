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

(spec/def ::passport
  (spec/keys :req-un [::ecl ::byr ::iyr ::hgt ::pid ::hcl ::eyr]
             :opt-un [::cid]))

;; part 1
(comment
  (println (count (filter #(spec/valid? ::passport %) input))))

(spec/def ::byr #(<= 1920 (Integer/parseInt %) 2002))
(spec/def ::iyr #(<= 2010 (Integer/parseInt %) 2020))
(spec/def ::eyr #(<= 2020 (Integer/parseInt %) 2030))
(spec/def ::hgt #(let [[matched? height units] (re-matches #"(\d+)(cm|in)" %)]
                   (and matched?
                        (case units
                          "cm" (<= 150 (Integer. height) 193)
                          "in" (<= 59 (Integer. height) 76)))))
(spec/def ::hcl #(re-matches #"\#[0-9a-f]{6}" %))
(spec/def ::ecl #{"brn" "oth" "hzl" "gry" "blu" "amb" "grn"})
(spec/def ::pid #(re-matches #"\d{9}" %))

;; part 2
(comment
  (println (count (filter #(spec/valid? ::passport %) input))))
