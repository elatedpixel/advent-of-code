(ns advent-2020.day05
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn- boarding-pass-id
  [row column]
  (+ (* row 8) column))

(defn- boarding-pass->binary
  [row-string]
  (s/replace row-string #"[BFLR]" {"F" "0" "B" "1" "L" "0" "R" "1"}))

(defn- binary-string->decimal
  [binary-string]
  (Integer/parseInt binary-string 2))

(t/with-test

  (defn decode-seat [boarding-pass]
    (let [[valid? row-string column-string] (re-matches #"([FB]{7})([LR]{3})" boarding-pass)
          row (binary-string->decimal (boarding-pass->binary row-string))
          column (binary-string->decimal (boarding-pass->binary column-string))]
      [row column (boarding-pass-id row column)]))

  (t/is (= [44 5 357] (decode-seat "FBFBBFFRLR")))
  (t/is (= [70 7 567] (decode-seat "BFFFBBFRRR")))
  (t/is (= [14 7 119] (decode-seat "FFFBBBFRRR")))
  (t/is (= [102 4 820] (decode-seat "BBFFBBFRLL"))))

(def input
  (line-seq (io/reader (io/resource "2020/day05"))))

;; part 1
(comment
  (time (println (apply max-key #(get % 2) (map decode-seat input)))))

;; part 2
(comment
  (let [passes (map decode-seat input)
        passmap (zipmap (map last passes) passes)]
    (keep identity
          (for [r (range 13 107)
                c (range 0 8)
                :let [id (boarding-pass-id r c)]]
            (if-not (passmap id) [r c id]))))
                                        ;
  )

(t/run-tests 'advent-2020.day05)
