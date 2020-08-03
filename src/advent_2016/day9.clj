(ns advent-2016.day9
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

  (t/are [input expected] (= expected (count (decompress input)))
    ;; ADVENT contains no markers and decompresses to itself with no changes,
    ;; resulting in a decompressed length of 6.
    "advent" 6

    ;; A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a
    ;; decompressed length of 7.
    "A(1x5)BC" 7

    ;; (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
    "(3x3)XYZ" 9

    ;; A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a
    ;; decompressed length of 11.
    "A(2x2)BCD(2x2)EFG" 11

    ;; (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but
    ;; because it's within a data section of another marker, it is not treated
    ;; any differently from the A that comes after it. It has a decompressed
    ;; length of 6.
    "(6x1)(1x3)A" 6

    ;; X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of
    ;; 18), because the decompressed data from the (8x2) marker (the (3x3)ABC)
    ;; is skipped and not processed further.
    "X(8x2)(3x3)ABCY" 18)

                                        ;
  )

(t/with-test

  (defn decompress-recursive-length [s]
    (if (seq s)
      (if-let [[matched? m n] (re-find #"^\((\d+)x(\d+)\)" s)]
       (let [subsequent-letters (Integer/parseInt m)
             repeat-count       (Integer/parseInt n)
             skip               (count matched?)
             text               (subs s skip (+ skip subsequent-letters))]
         (+ (* repeat-count (decompress-recursive-length text))(decompress-recursive-length (subs s (+ skip subsequent-letters)))))
       (+ 1 (decompress-recursive-length (subs s 1))))
      0))

  ;; (defn decompress-recursive [s]
  ;;   (when (seq s)
  ;;     (if-let [[matched? n x] (re-find #"^\((\d+)x(\d+)\)" s)]
  ;;       (let [subsequent-letters (Integer/parseInt n)
  ;;             repeat-count       (Integer/parseInt x)
  ;;             skip               (count matched?)]
  ;;         (lazy-cat
  ;;          (flatten (repeat repeat-count (decompress-recursive (subs s skip (+ skip subsequent-letters)))))
  ;;          (decompress-recursive (subs s (+ skip subsequent-letters)))))
  ;;       (lazy-seq
  ;;        (cons (first s)
  ;;              (decompress-recursive (subs s 1))))))   )

  (t/are [input expected] (= expected (decompress-recursive-length input))
    ;; (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no
    ;; markers. (9)
    "(3x3)XYZ" 9

    ;; X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data
    ;; from the (8x2) marker is then further decompressed, thus triggering the
    ;; (3x3) marker twice for a total of six ABC sequences. (20)
    "X(8x2)(3x3)ABCY" 20

    ;; (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated
    ;; 241920 times.
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" 241920

    ;; (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445
    ;; characters long.
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" 445
                                        ;
    )
                                        ;
  )

(defn -main []
  (let [data (-> "2016/day9.txt"
                 io/resource
                 slurp
                 str/trim)]

    ; part 1
    (time (println (count (decompress data))))

    ; part 2
    (time (println (decompress-recursive-length data)))))

(t/run-tests 'advent-2016.day9)
