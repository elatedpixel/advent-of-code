(ns advent-2016.day5
  (:require [clojure.java.io :as io])
  (:import java.security.MessageDigest
           java.math.BigInteger))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn get-next-password-character [s]
  (if (.startsWith s "00000")
    (get s 5)
    nil))

(defn get-password [s]
  (keep get-next-password-character
        (map (comp md5 (partial str s)) (range))))

(def code (atom {}))
(defn get-code [s]
  (if-let [[found? position character] (re-matches #"^[0]{5}(\d)(\S).*$" s)]
    (let [i (Integer/parseInt position)]
      (if (and
           (not (@code i))
           (<= 0 i 7))
        (swap! code assoc i character)))))

(defn hashes
  ([s] (hashes s 0))
  ([s n] (lazy-seq (cons (md5 (str s n)) (hashes s (inc n))))))

(def data
  (-> "2016/day5.txt"
      io/resource
      io/reader
      line-seq
      first))

(defn -main []
  #_(time (println (take 8 (get-password data))))
  (time (println (->> data
                      hashes
                      (keep get-code)
                      (take-while (fn [_] (not (every? @code (range 4)))))
                      #_(sort-by key)
                      #_(map second)))))

#_(-main)
