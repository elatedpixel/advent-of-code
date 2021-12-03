(ns advent-2021.day02
  (:require
   [advent.core :refer [load-input string->sexpression]]))

(def input (load-input 2021 2))
(def commands (map string->sexpression input))

(def initial-state
  {:horizontal 0
   :depth      0
   :aim        0})

(defn pilot-submarine [pilot]
  (reduce pilot initial-state commands))

(defn distance [{:keys [horizontal depth]}]
  (* horizontal depth))

(defn silver-pilot [position [command x]]
  (case command
    forward (update position :horizontal + x)
    down    (update position :depth + x)
    up      (update position :depth - x)))

;; silver
(distance (pilot-submarine silver-pilot));; => 1636725

(defn gold-pilot [{:keys [aim] :as position} [command x]]
  (case command
    forward (-> position
                (update :horizontal + x)
                (update :depth + (* aim x)))
    down    (update position :aim + x)
    up      (update position :aim - x)))

;; gold
(distance (pilot-submarine gold-pilot));; => 1872757425
