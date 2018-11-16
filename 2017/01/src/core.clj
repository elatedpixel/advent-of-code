(ns core)

(defn parse-numbers [s]
  (loop [n (read-string s)
         r []]
    (if (zero? n) r
        (recur (quot n 10) (conj r (rem n 10))))))

(defn sum-matching-pairs [s]
  (let [n (parse-numbers s)] 
    (->> n
         (partition 2 1 (take 1 n))
         (reduce (fn [r [a b]] (+ r (if (= a b) a 0))) 0))))

(defn -main []
  (println (sum-matching-pairs (slurp "./input"))))
