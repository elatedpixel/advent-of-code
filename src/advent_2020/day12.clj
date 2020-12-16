(ns advent-2020.day12
  (:require [clojure.java.io :as io]))

(def sample-input "F10
N3
F7
R90
F11")

(defn- parse-navigation
  [instructions]
  (re-seq #"([NSEWLRF])(\d+)" instructions))

(defn coord+
  ([x] x)
  ([x y] (mapv + x y))
  ([x y & more] (reduce coord+ (coord+ x y) more)))

(defn coord*
  ([x] x)
  ([x y] (if (number? x)
           (coord* (into [] (repeat (count y) x)) y)
           (mapv * x y))))

(def direction
  {:N [-1 0]
   :E [0 1]
   :S [1 0]
   :W [0 -1]})

(defn turn [coord offset]
  (direction
   (nth
    (case coord
      [-1 0] [:N :E :S :W]
      [0 1]  [:E :S :W :N]
      [1 0]  [:S :W :N :E]
      [0 -1] [:W :N :E :S])
    offset)))

(defn op [operation]
  (case operation
    :R (fn [{:keys [facing] :as ship} value] (update ship :facing turn (quot value 90)))
    :L (fn [{:keys [facing] :as ship} value] (update ship :facing turn (- 4 (quot value 90))))
    :F (fn [{:keys [facing] :as ship} value] (update ship :coord coord+ (coord* value facing)))
    (:E :W :S :N) (fn [ship value] (update ship :coord coord+ (coord* value (direction operation))))))

(defn manhattan-distance [{:keys [E W N S]}]
  (+ (+ (Math/abs E) (Math/abs W))
     (+ (Math/abs S) (Math/abs N))))

(defn- follow-instructions
  [instructions state]
  (transduce
   (map (fn [[matched? op-string n-string]]
          {:operation (op (keyword op-string))
           :value     (Integer/parseInt n-string)}))
   (completing (fn [ship {:keys [operation value]}]
                 (operation ship value)))
   state
   (parse-navigation instructions)))

;; part 1
(comment
  (let [input (slurp (io/resource "2020/day12"))]
    (time (println (follow-instructions input {:coord [0 0] :facing [0 1]})))))

;; part 2
(comment
  (defn turn [coord offset]
    (nth (iterate (fn [[y x]] [x (- y)]) coord) offset))

  (defn op [operation]
    (case operation
      :R (fn [ship value] (update ship :waypoint turn (quot value 90)))
      :L (fn [ship value] (update ship :waypoint turn (- 4 (quot value 90))))
      :F (fn [ship value] (update ship :coord coord+ (coord* value (:waypoint ship))))
      (:E :W :S :N) (fn [ship value] (update ship :waypoint coord+ (coord* value (direction operation))))))

  (with-redefs [op op
                turn turn]
    (let [input (slurp (io/resource "2020/day12"))]
      (time (println (follow-instructions input {:coord [0 0] :waypoint [-1 10]})))))
  )
