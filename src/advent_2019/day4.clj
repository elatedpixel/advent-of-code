(ns advent-2019.day4)

;; Your puzzle input is 248345-746315.
(def input (range 248345 (inc 746315)))

(defn six-digit-number? [n]
  (= 6 (count (str n))))

(defn contains-contiguous-duplicate? [n]
  (some (fn [[a b]] (= a b))
        (partition 2 1 (str n))))

(defn contains-contiguous-unique-duplicate? [n]
  (let [counts (frequencies (str n))]
    (some (fn [[a b]] (and (= a b) (= 2 (counts a))))
          (partition 2 1 (str n)))))

(defn increasing-order? [n]
  (apply <= (map int (str n))))

(def valid-password-part1?
  (every-pred six-digit-number?
              contains-contiguous-duplicate?
              increasing-order?))

(def valid-password-part2?
  (every-pred six-digit-number?
              contains-contiguous-unique-duplicate?
              increasing-order?))

(defn part1 []
  (time (println (count (filter valid-password-part1? input)))))
;; 1019
;; "Elapsed time: 2540.0887 msecs"

(defn part2 []
  (time (println (count (filter valid-password-part2? input)))))
;; 660
;; "Elapsed time: 3388.1456 msecs"
