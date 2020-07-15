(ns advent-2016.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.test :as t]))

(def data
  (-> "2016/day7.txt"
      io/resource
      io/reader
      line-seq))

(defn abba? [[a b c d]]
  (and (= a d)
       (= b c)
       (not= a b)))

(defn tls? [[outside-brackets inside-brackets]]
  (and (some abba? (mapcat #(partition 4 1 %) outside-brackets))
       (not (some abba? (mapcat #(partition 4 1 %) inside-brackets)))))

(comment
  ;; part 1
  (time (count (sequence
                (comp
                 (map #(s/split % #"\W"))
                 (map (juxt #(take-nth 2 %) #(take-nth 2 (rest %))))
                 (filter tls?))
                data)))
                                        ;
  )

(defn aba? [[a b c]]
  (and (= a c)
       (not= a b)))

(defn ssl? [[outside-brackets inside-brackets]]
  (seq (clojure.set/intersection
    (into #{}
          (comp (mapcat #(partition 3 1 %))
                (filter aba?)
                (map (fn [[a b c]] (str b a b))))
          outside-brackets)
    (into #{}
          (comp (mapcat #(partition 3 1 %))
                (filter aba?)
                (map #(apply str %)))
          inside-brackets))))

(comment
  ;; part 2
  (time
   (count (sequence
     (comp
      (map #(s/split % #"\W"))
      (map (juxt #(take-nth 2 %) #(take-nth 2 (rest %))))
      (filter ssl?))
     data)))
                                        ;
         )

(t/run-tests 'advent-2016.day7)
