(ns advent-2018.day4
  (:require [clojure.test :refer [with-test is run-tests]]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn- parse-logs [sleep-logs]
  (loop [logs sleep-logs
         last-id nil
         r []]
    (if (empty? logs) r
        (let [[[type] [year month day hour minute id]] (first logs)]
          (recur (rest logs) (or id last-id)
                 (conj r (cond-> {:date [year month day]
                                  :time [hour minute]
                                  :type (keyword (clojure.string/lower-case type))}
                           last-id (assoc :id last-id)
                           id (assoc :id id))))))))

(defn- sleep-intervals [sleep-logs]
  (for [[k v] sleep-logs]
    [k (->> (group-by :date v)
            vals
            (mapcat #(->> %
                          (map (comp second :time))
                          (partition 2)
                          (map (fn [pair] (apply range pair))))))]))

(time (def data
       (->> ((comp line-seq io/reader io/resource) "2018/day4.txt")
            (map (juxt (some-fn
                        (partial re-seq #"Guard")
                        (partial re-seq #"falls")
                        (partial re-seq #"wakes"))
                       #(map (fn [n] (Integer/parseInt n)) (re-seq #"\d+" %))))
            parse-logs
            (remove #(= (:type %) :guard))
            (group-by :id)
            sleep-intervals
            (into {}))))
(println "Parsed data..")

#_(run-tests 'advent-2018.day4)

(defn strategy-1 []
  (let [guard (key (apply max-key (fn [[k v]] (reduce + (map count v))) data))
        minute (key (apply max-key val (apply merge-with + (map frequencies (get data guard)))))]
    (* guard minute)))

(defn strategy-2 []
  (reduce * (apply max-key val
                   (reduce (fn [m [k v]] (assoc m k (->> (map frequencies v)
                                                         (apply merge-with +)
                                                         (apply max-key val)
                                                         key)))
                           {}
                           advent-2018.day4/data))))

(defn -main []
  (prn)
  (println (str "Day 4 Part 1: " (time (strategy-1))))
  (println (str "Day 4 Part 2: " (time (strategy-2)))))
