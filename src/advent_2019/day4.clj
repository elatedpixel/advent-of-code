(ns advent-2019.day4)

;; Your puzzle input is 248345-746315.
(def input (range 248345 (inc 746315)))

(defn six-digit-number? [n]
  (= 6 (count (str n))))

(defn contains-congruent-duplicate? [n]
  (some (fn [[a b]] (= a b)) (partition 2 1 (str n))))

(defn increasing-order? [n]
  (apply <= (map int (str n))))

(def valid-password?
  (every-pred six-digit-number?
              contains-congruent-duplicate?
              increasing-order?))

(defn part1 []
  (time (println (count (filter valid-password? input)))))
