(ns advent-2021.day17
  (:require
   [advent.core :as core]))

(def example "target area: x=20..30, y=-10..-5")
(def input-string (first (core/load-input 2021 17)))

(defn parse-input [s]
  (let [[xmin xmax ymin ymax]
        (map #(Integer/parseInt %)
             (rest (re-find #"target area: x=(?<xmin>-?\d+)..(?<xmax>-?\d+), y=(?<ymin>-?\d+)..(?<ymax>-?\d+)" s)))]
    {:x [xmin xmax]
     :y [ymin ymax]}))

(let [yp (get-in (parse-input input-string) [:y 0])]
  (quot (* yp (inc yp)) 2));; => 2278

(defn step [position velocity]
  [(-> position
       (update 0 + (velocity 0))
       (update 1 + (velocity 1)))
   (-> velocity
       (update 0 + (cond
                     (pos? (velocity 0)) -1
                     (neg? (velocity 0)) 1
                     :else               0))
       (update 1 dec))])

(defn fire [velocity x y]
  (loop [[position velocity] [[0 0] velocity]]
    (cond
      (and (<= (x 0) (position 0) (x 1))
           (<= (y 0) (position 1) (y 1))) 1
      (or (> (position 0) (x 1))
          (< (position 1) (y 0)))         0
      :else                               (recur (step position velocity)))))

(let [{:keys [x y]} (parse-input input-string)]
  (reduce +
          (for [xv (range (inc (x 1)))
                yv (range (y 0) (inc (- -1 (y 0))))]
            (fire [xv yv] x y))))
;; => 996
