(ns advent-2018.day5
  (:require [clojure.test :refer [with-test is run-tests]]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def data (string/trim (slurp (io/resource "2018/day5.txt"))))

(with-test
  (defn polymer? [a b]
    (= 32 (Math/abs (- (int a) (int b)))))
  (is (true? (polymer? \a \A)))
  (is (true? (polymer? \Z \z)))
  (is (false? (polymer? \c \c))))

(with-test
  (defn chain-reaction [s]
    (loop [result '()
           [a b & coll] s]
      (cond (nil? a) result
            (nil? b) (cons a result)
            (polymer? a b) (recur (rest result) (cond-> coll
                                                  (not-empty result) (conj (first result))))
            :else (recur (cons a result) (cons b coll)))))
  (is (= (reverse (seq "dabCBAcaDA")) (chain-reaction "dabAcCaCBAcCcaDA")))
  (is (= (reverse '(\z \g \g)) (chain-reaction "zBaDddZzDAbgg"))))

(with-test
  (defn remove-chars [s c]
    (let [re (re-pattern (format "(?i)[^%s]" c))]
      (seq (apply str (re-seq re s)))))
  (is (= (seq "dbcCCBcCcD") (remove-chars "dabAcCaCBAcCcaDA" "a"))))

(with-test
  (defn improve-polymer [s]
    (->> (distinct (string/lower-case s))
         (map (partial remove-chars s))
         (map (comp count chain-reaction))
         (reduce min)))
  (is (= 4 (improve-polymer "dabAcCaCBAcCcaDA"))))

(run-tests 'advent-2018.day5)

(defn -main [& args]
  (prn)
  (println (str "Day 5 Part 1: " (time (count (chain-reaction data)))))
  (println (str "Day 5 Part 2: " (time (improve-polymer data)))))
