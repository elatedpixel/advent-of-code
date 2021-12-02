(ns advent-2021.day02
  (:require [advent.core :refer [load-input]]))

(def input (load-input 2021 2))

(declare ^:dynamic forward)
(declare ^:dynamic down)
(declare ^:dynamic up)

(declare ^:dynamic aim)
(declare ^:dynamic depth)
(declare ^:dynamic horizontal)

(defn execute-commands [commands]
  (doseq [command commands]
   (eval command)))

(def commands (map (comp read-string (partial format "(%s)")) input))

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
