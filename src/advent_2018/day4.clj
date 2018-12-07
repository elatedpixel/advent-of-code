(ns advent-2018.day4
  (:require [clojure.test :refer [with-test is run-tests]]
            [clojure.java.io :as io]))

(defn str->int [s]
  (Integer/parseInt s 10))

(with-test

  (defn parse-log [s]
    (let [[year month day hour minute event]
          (rest (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" s))]
      {:time (mapv str->int [year month day hour minute])
       :event event}))

  (is (= {:time [1518 11 1 0 0] :event "falls asleep"}
         (parse-log "[1518-11-01 00:00] falls asleep")))
  (is (= {:time [1518 11 21 0 1] :event "Guard #641 begins shift"}
         (parse-log "[1518-11-21 00:01] Guard #641 begins shift"))))

(with-test

  (defn parse-event [s]
    (if-let [guard (last (re-find #"\w+ #(\d+)" s))]
      {:guard (str->int guard) :sleeping? false}
      {:sleeping? (boolean (re-find #"falls" s))}))

  (is (= {:guard 641 :sleeping? false} (parse-event "Guard #641 begins shift")))
  (is (= {:sleeping? true} (parse-event "falls asleep")))
  (is (= {:sleeping? false} (parse-event "wakes up"))))

(with-test

  (defn fill-minutes [minutes [minute sleep]]
    (merge minutes
           {minute sleep}
           (let [[m s] (if (empty? minutes) [0 (- 1 sleep)]
                           (apply max-key key minutes))]
               (into {} (mapv vector (range m minute) (repeat s))))))

  (is (= {0 0 1 0 2 0 3 1} (fill-minutes {0 0} [3 1])))
  (is (= {0 0 1 0 2 0 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 0} (fill-minutes {0 0 1 0 2 0 3 1} [10 0]))))

(with-test

  (defn make-sleep-map [coll]
    (loop [[log & logs] coll
           last-guard nil
           m {}]
      (if (nil? log) m
          (let [[year month day hour minute] (:time log)
                event (parse-event (:event log))
                sleep (if (:sleeping? event) 1 0)
                guard (or (:guard event) last-guard)]
            (recur logs guard (update-in m [guard day] fill-minutes [minute sleep]))))))

  (is (= {641 {01 {0 0 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 1 11 0}
               02 {0 0 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 1 11 0}}}
         (make-sleep-map (map parse-log ["[1518-11-01 00:00] Guard #641 begins shift"
                                         "[1518-11-01 00:01] falls asleep"
                                         "[1518-11-01 00:11] wakes up"
                                         "[1518-11-02 00:01] falls asleep"
                                         "[1518-11-02 00:11] wakes up"])))))

(defn sum-sleeping [[elf days]]
  (prn elf (apply + (vals (apply merge-with + (map val days)))))
  (apply + (vals (apply merge-with + (map val days)))))

(defn max-sleeping-minute [days]
  (apply merge-with + (vals days)))

(defn strategy-1 [logs]
  "returns (* guard minute) where guard is the one that sleeps the most
  and minute is the one where that guard is sleeping most often"
  (let [[guard totalminutes] (apply max-key sum-sleeping logs)
        [minute count] (apply max-key val (max-sleeping-minute (get logs guard)))]
    (prn guard, totalminutes)
    (prn minute, count)
    (* guard minute)))

(run-tests 'advent-2018.day4)

(defn -main []
  (let [input (make-sleep-map (map parse-log (line-seq (io/reader (io/resource "2018/day4.txt")))))]
    (prn)
    (println (str "Day 4 Part 1: " (time (strategy-1 input))))))
