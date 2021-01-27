(ns advent-2015.day04)

(defn md5 [^String s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(def input "yzbqklnj")

(comment
  ;; part 1
  (time
   (println
    (first (for [n     (range)
                 :when (re-matches #"^[0]{5}.*$" (md5 (str input n)))] n)))))

(comment
  ;; part 2
  (time
   (println
    (first (for [n     (range)
                 :when (re-matches #"^[0]{6}.*$" (md5 (str input n)))] n)))))
