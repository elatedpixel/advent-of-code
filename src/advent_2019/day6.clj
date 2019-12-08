(ns advent-2019.day6
  (:require [advent-2019.core :refer [read-lines]]))

#_(defn make-tree [m root]
    (lazy-seq
     (cons root
           (map (partial make-tree m) (m root)))))

(defn make-parent-lookup [input]
  (into {} (mapv #((comp vec reverse)
                   (map symbol (clojure.string/split % #"\)"))) input)))

(defn child-path [m x]
  (take-while identity (iterate m x)))

(defn part1 []
  (let [input (read-lines "2019/day6.txt")
        m (make-parent-lookup input)]
    (->> (keys m)
         (map (comp rest (partial child-path m)))
         (map count)
         (reduce +))))

(defn part2 []
  (let [input (read-lines "2019/day6.txt")
        m (make-parent-lookup input)
        you-path (rest (child-path m 'YOU))
        san-path (rest (child-path m 'SAN))]
    (apply min (map #(+ (.indexOf you-path %) (.indexOf san-path %))
                    (clojure.set/intersection (set you-path) (set san-path))))))

(comment
  (part1)
  ;; => 160040

  (part2)
  ;; => 373
  )
