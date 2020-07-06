(ns advent-2016.day7
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def data
  (-> "2016/day7.txt"
      io/resource
      io/reader
      line-seq))

(defn mirrored? [[a b c d]]
  (and (= a d)
       (= b c)))

(defn tls? [ip]
  (boolean (some mirrored? (partition 4 1 ip))))

(defn tls-rf
  [supports-tls? [found? ip hypernet]]
  (if (and (some? hypernet) (tls? hypernet))
    (reduced false)
    (or supports-tls? (and (some? ip) (tls? ip)))))

(defn split-ip [s]
  (re-seq #"(\w+)|\[(\w+)\]" s))

(comment (->> data
              (map split-ip)
              (filter #(reduce tls-rf false %))
              count)

         ;; => ("rhamaeovmbheijj[hkwbkqzlcscwjkyjulk]ajsxfuemamuqcjccbc")

         ;; => ((["rhamaeovmbheijj" "rhamaeovmbheijj" nil]
         ;;      ["[hkwbkqzlcscwjkyjulk]" nil "hkwbkqzlcscwjkyjulk"]
         ;;      ["ajsxfuemamuqcjccbc" "ajsxfuemamuqcjccbc" nil]))
                                        ;


         (autonomous-bridge-bypass-annotation? "rhamaeovmbheijj[hkwbkqzlcscwjkyjulk]ajsxfuemaamuqcjccbc")
                                        ;
         )

(t/run-tests 'advent-2016.day7)
