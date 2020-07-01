(ns advent-2016.day6
  (:require [clojure.java.io :as io]))

(def data
  (-> "2016/day6.txt"
      io/resource
      io/reader
      line-seq))

(defn signal [data]
  (map #(key (apply max-key val %))
       (map frequencies (apply map str data))))

(defn signal-2 [data]
  (map #(key (apply min-key val %))
       (map frequencies (apply map str data))))

(comment
  (time (apply str (signal data)))
  "Elapsed time: 5.321485 msecs"
  ;; => "qtbjqiuq"
                                        ;

  (time (apply str (signal-2 data)))
  ;; => "akothqli"
  "Elapsed time: 5.524848 msecs"
                                        ;
  )
