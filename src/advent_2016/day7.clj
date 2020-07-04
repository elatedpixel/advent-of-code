(ns advent-2016.day7
  (:require [clojure.java.io :as io]))

(def data
  (-> "2016/day7.txt"
      io/resource
      io/reader
      line-seq))

(defn mirrored? [[a b c d]]
  (and (= a d)
       (= b c)))

(defn tls?
  ([ip]
   (some mirrored? (partition 4 1 ip)))
  ([found? ip hypernet]
   (and found?
        (and (tls? ip)
             (not (tls? hypernet))))))

(comment (map tls? (map #(re-seq #"(\w+)|\[(\w+)\]" %) (take 30 data)))

         ;; => ("rhamaeovmbheijj[hkwbkqzlcscwjkyjulk]ajsxfuemamuqcjccbc")

         ;; => ((["rhamaeovmbheijj" "rhamaeovmbheijj" nil]
         ;;      ["[hkwbkqzlcscwjkyjulk]" nil "hkwbkqzlcscwjkyjulk"]
         ;;      ["ajsxfuemamuqcjccbc" "ajsxfuemamuqcjccbc" nil]))
                                        ;
         )
