(ns advent-2019.day5
  (:require [advent-2019.core :refer [read-lines]]
            [clojure.test :as t]))

(def n-params
  {99 1
   1  4
   2  4
   3  2
   4  2
   5  3
   6  3
   7  4
   8  4})

(defn parse-instruction [n]
  [(rem n 100) (reverse (format "%03d" (quot n 100)))])

(defmulti execute (fn [op & _] op))

(defmethod execute 99 [_ state & _]
  (assoc state :halt? true))

(defmethod execute 1 [op {:keys [pointer program] :as state} [a b address]]
  (-> (assoc-in state [:program address] (+ (program a) (program b)))
      (update :pointer + (n-params op))))

(defmethod execute 2 [op {:keys [pointer program] :as state} [a b address]]
  (-> (assoc-in state [:program address] (* (program a) (program b)))
      (update :pointer + (n-params op))))

(defmethod execute 3 [op {:keys [pointer program] :as state} [address]]
  (-> (assoc-in state [:program address] (Integer/parseInt (read-line)))
      (update :pointer + (n-params op))))

(defmethod execute 4 [op {:keys [pointer program] :as state} [address]]
  (do
    (println (program address))
    (-> (assoc state :output (program address))
        (update :pointer + (n-params op)))))

(defmethod execute 5 [op {:keys [pointer program] :as state} [a jump]]
  (assoc state :pointer (if (not (zero? (program a)))
                          (program jump)
                          (+ (n-params op) pointer))))

(defmethod execute 6 [op {:keys [pointer program] :as state} [a jump]]
  (assoc state :pointer (if (zero? (program a))
                          (program jump)
                          (+ (n-params op) pointer))))

(defmethod execute 7 [op {:keys [pointer program] :as state} [a b address]]
  (-> (assoc-in state [:program address] (if (< (program a) (program b)) 1 0))
      (update :pointer + (n-params op))))

(defmethod execute 8 [op {:keys [pointer program] :as state} [a b address]]
  (-> (assoc-in state [:program address] (if (= (program a) (program b)) 1 0))
      (update :pointer + (n-params op))))

(t/with-test

  (defn computer [{:keys [program pointer halt?] :as state}]
    (if halt?
      state
      (letfn [(value [[mode n]] (if (= mode \0) (program n) n))]
        (let [[op modes] (parse-instruction (program pointer))
              [instr & params] (map #(+ % pointer) (range (n-params op)))
              values (map (comp value vector) modes params)]
          (recur (execute op state values))))))

  (t/is (= [1002 4 3 4 99]
           (:program
            (computer {:halt?   false
                       :output  0
                       :pointer 0
                       :program [1002 4 3 4 33]}))))

  ; computer
  )

(t/run-tests 'advent-2019.day5)

(def input
  (->> (read-lines "2019/day5.txt")
       first
       (format "(%s)")
       read-string
       vec))

(defn start []
  (computer {:halt?   false
             :output  0
             :pointer 0
             :program input}))
