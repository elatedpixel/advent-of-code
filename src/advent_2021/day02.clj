(ns advent-2021.day02
  (:require [advent.core :refer [load-input]]))

(def input (load-input 2021 2))

;; commands in the input text
(declare ^:dynamic forward)
(declare ^:dynamic down)
(declare ^:dynamic up)

;; variables modified by commands
(declare ^:dynamic aim)
(declare ^:dynamic depth)
(declare ^:dynamic horizontal)

;; format commands as s-expressions
(def commands (map (comp read-string (partial format "(%s)")) input))

(defn execute-commands [commands]
  (doseq [command commands]
    (eval command)))

;; part 1
(binding [forward (fn [n] (swap! horizontal + n))
          down (fn [n] (swap! depth + n))
          up (fn [n] (swap! depth - n))
          horizontal (atom 0)
          depth (atom 0)]
  (do (execute-commands commands)
      (* @horizontal @depth)))
;; => 1636725

;; part 2
(binding [forward (fn [n] (do
                            (swap! horizontal + n)
                            (swap! depth + (* @aim n))))
          down (fn [n] (swap! aim + n))
          up (fn [n] (swap! aim - n))
          horizontal (atom 0)
          depth (atom 0)
          aim (atom 0)]
  (do (execute-commands commands)
      (* @horizontal @depth)))
;; => 1872757425
