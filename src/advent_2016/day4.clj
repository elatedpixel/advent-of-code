(ns advent-2016.day4
  (:require [clojure.java.io :as io]
            [clojure.test :as t]))

(def data
  (-> "2016/day4.txt"
      io/resource
      io/reader
      line-seq))

(t/with-test

  (defn get-checksum [s]
    (let [freq (frequencies s)]
      (apply str
             (take 5
                   (sort-by
                    (juxt #(- (freq %)) identity)
                    (keys freq))))))

  (t/is (= "abxyz" (get-checksum "aaaaabbbzyx"))))

(t/with-test

  (defn parse-room [s]
    (let [[parsed? room sector checksum] (re-matches #"([a-z-]+)(\d+)\[(\w+)\]" s)]
      [(apply str (re-seq #"[a-z]+" room))
       (Integer/parseInt sector)
       checksum]))

  (t/is (= ["aaaaabbbzyx" 123 "abxyz"]
           (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))))

(t/with-test

  (defn real-room? [[room sector checksum]]
    (= (get-checksum room) checksum))

  (t/is (= true (real-room? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))))
  (t/is (= true (real-room? (parse-room "a-b-c-d-e-f-g-h-987[abcde]"))))
  (t/is (= true (real-room? (parse-room "not-a-real-room-404[oarel]"))))
  (t/is (= false (real-room? (parse-room "totally-real-room-200[decoy]")))))

(defn part1 [data]
  (->> data
       (map parse-room)
       (filter real-room?)
       (map second)
       (reduce +)))

(defn -main []
  (time (println (part1 data))))

(t/run-tests 'advent-2016.day4)
