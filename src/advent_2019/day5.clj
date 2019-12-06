(ns advent-2019.day5
  (:require [advent-2019.core :refer [read-lines]]
            [clojure.test :as t]))

(defn opcode [instruction]
  (rem instruction 100))

(defn value [program [mode n]]
  (if (= mode \0) (get program n) n))

(defmulti execute
  (fn [{:keys [program pointer]}]
    (opcode (program pointer))))

(defmethod execute 99 [state]
  (assoc state :halt? true))

(defmethod execute 1 [{:keys [pointer program] :as state}]
  (let [[instr a b address] (subvec program pointer (+ pointer 4))
        modes               (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-b]     (map vector modes [a b])]
    (-> state
        (assoc-in [:program address] (+ (value program mode-a) (value program mode-b)))
        (update :pointer + 4))))

(defmethod execute 2 [{:keys [pointer program] :as state}]
  (let [[instr a b address] (subvec program pointer (+ pointer 4))
        modes               (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-b]     (map vector modes [a b])]
    (-> state
        (assoc-in [:program address] (* (value program mode-a) (value program mode-b)))
        (update :pointer + 4))))

(defmethod execute 3 [{:keys [pointer program] :as state}]
  (let [input (Integer/parseInt (read-line))
        [instr address] (subvec program pointer (+ pointer 2))]
    (-> state
        (assoc-in [:program address] input)
        (update :pointer + 2))))

(defmethod execute 4 [{:keys [pointer program] :as state}]
  (let [[instr address] (subvec program pointer (+ pointer 2))]
    (do
      (println (program address))
      (-> state
          (assoc :output (program address))
          (update :pointer + 2)))))

(defmethod execute 5 [{:keys [pointer program] :as state}]
  (let [[instr a jump]     (subvec program pointer (+ pointer 3))
        modes              (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-jump] (map vector modes [a jump])]
    (-> state
        (assoc :pointer (if (not (zero? (value program mode-a)))
                          (value program mode-jump)
                          (+ 3 pointer))))))

(defmethod execute 6 [{:keys [pointer program] :as state}]
  (let [[instr a jump]     (subvec program pointer (+ pointer 3))
        modes              (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-jump] (map vector modes [a jump])]
    (-> state
        (assoc :pointer (if (zero? (value program mode-a))
                          (value program mode-jump)
                          (+ 3 pointer))))))

(defmethod execute 7 [{:keys [pointer program] :as state}]
  (let [[instr a b address] (subvec program pointer (+ pointer 4))
        modes               (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-b]     (map vector modes [a b])]
    (-> state
        (assoc-in [:program address] (if (< (value program mode-a) (value program mode-b)) 1 0))
        (update :pointer + 4))))

(defmethod execute 8 [{:keys [pointer program] :as state}]
  (let [[instr a b address] (subvec program pointer (+ pointer 4))
        modes               (reverse (format "%03d" (quot instr 100)))
        [mode-a mode-b]     (map vector modes [a b])]
    (-> state
        (assoc-in [:program address] (if (= (value program mode-a) (value program mode-b)) 1 0))
        (update :pointer + 4))))

(t/with-test

  (defn computer [{:keys [halt?] :as state}]
    (if halt?
      state
      (recur (execute state))))

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
