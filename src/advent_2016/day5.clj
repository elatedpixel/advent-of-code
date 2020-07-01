(ns advent-2016.day5
  (:require [clojure.java.io :as io]
            [digest :refer (md5)]))

(def data
  (-> "2016/day5.txt"
      io/resource
      io/reader
      slurp))

(defn get-password [s]
  (transduce
   (comp
    (map #(md5 (str s %)))
    (filter #(= "00000" (subs % 0 5)))
    (map #(nth % 5))
    (take 8))
   conj
   []
   (range)))

(defn distinct-by [f]
  (fn [xf]
    (let [seen (volatile! #{})]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (@seen (f input))
           result
           (do
             (vswap! seen conj (f input))
             (xf result input))))))))

(defn get-password-2 [s]
  (transduce
   (comp
    (map #(md5 (str s %)))
    (filter #(and
              (= "00000" (subs % 0 5))
              (> 8 (Integer/parseInt (subs % 5 6) 16))))
    (map (juxt #(Integer/parseInt (subs % 5 6) 16)
               #(nth % 6)))
    (distinct-by first)
    (take 8))
   conj
   []
   (range)))

(defn -main []
  (time (get-password data))
  ;; "Elapsed time: 48706.07252 msecs"
  ;; => [\8 \1 \5 \5 \7 \7 \b \e]
                                        ;

  (time (let [codes (into {} (get-password-2 "ojvtpuvg"))]
          (vals (sort-by key codes))))
  ;; "Elapsed time: 109745.577176 msecs"
  ;; => (\1 \0 \5 \0 \c \b \b \d)
  )

#_(-main)
