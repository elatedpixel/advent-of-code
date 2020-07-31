(ns advent-2016.day9
  (:require [clojure.test :as t]))

(t/with-test

  (defn decompress [s]
    (when (seq s)
      (if-let [[matched? n x] (re-find #"^\((\d+)x(\d+)\)" s)]
        (let [subsequent-letters (Integer/parseInt n)
              repeat-count       (Integer/parseInt x)
              skip               (count matched?)]
          (lazy-cat
           (take (* subsequent-letters repeat-count)
                 (cycle (subs s skip (+ skip subsequent-letters))))
           (decompress (subs s (+ skip subsequent-letters)))))
        (lazy-seq
         (cons (first s)
               (decompress (subs s 1)))))))

  (t/testing "ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6."
    (let [input    "advent"
          actual   (decompress input)
          expected (seq input)]
      (t/is (= expected actual))))

  (t/testing "A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7."
    (let [input    "A(1x5)BC"
          actual   (decompress input)
          expected (seq "ABBBBBC")]
      (t/is (= expected actual))))

  (t/testing "(3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11."
    (let [input    "(3x3)XYZ"
          actual   (decompress input)
          expected (seq "XYZXYZXYZ")]
      (t/is (= expected actual))))

  (t/testing "(6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6."
    (let [input    "(6x1)(1x3)A"
          actual   (decompress input)
          expected (seq "(1x3)A")]
      (t/is (= expected actual))))

  (t/testing "X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further."
    (let [input    "X(8x2)(3x3)ABCY"
          actual   (decompress input)
          expected (seq "X(3x3)ABC(3x3)ABCY")]
      (t/is (= expected actual))))

                                        ;
  )

(t/run-tests 'advent-2016.day9)
