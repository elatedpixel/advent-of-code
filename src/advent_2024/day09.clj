(ns advent-2024.day09
  (:require
   [clojure.java.io :as io]
   [clojure.test :as t]))

(def ^:private sample0
  "2333133121414131402")
(def ^:private puzzle
  (slurp (io/resource "2024/day09.txt")))

(last puzzle)

(comment
  (partition-all 2 sample0)
  ;; => ((\2 \3) (\3 \3) (\1 \3) (\3 \1) (\2 \1) (\4 \1) (\4 \1) (\3 \1) (\4 \0) (\2))
  )

(defn- parse [input]
  (mapcat
    (fn [id [files freespace]]
      (concat
        (repeat (parse-long (str files)) id)
        (if (or (= \. freespace)
                (= \newline freespace))
          '()
          (repeat (parse-long (str freespace)) \.))))
    (range)
    (partition-all 2 input)))

(comment
  (parse sample0)
;; => (0 0 \. \. \. 1 1 1 \. \. \. 2 \. \. \. 3 3 3 \. 4 4 \. 5 5 5 5 \. 6 6 6 6 \. 7 7 7 \. 8 8 8 8 9 9)
  )

;; expects filesystem (vec "00...111...2...333.44.5555.6666.777.888899")
(defn- defrag
  "Move blocks from end of disk to fill in empty blocks."
  [filesystem]
  (loop [i  0
         j  (dec (count filesystem))
         fs (vec filesystem)]
    (cond
      (> i j)                    fs
      (not (number? (nth fs j))) (recur i (dec j) fs)
      (number? (nth fs i))       (recur (inc i) j fs)
      :else                      (do
                                   (println (str "swapping " i j))
                                   (recur (inc i)
                                          (dec j)
                                          (-> fs
                                              (assoc i (nth fs j))
                                              (assoc j (nth fs i))))))))

(comment
  (defrag (parse sample0))
;; => [0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6 \. \. \. \. \. \. \. \. \. \. \. \. \. \.]
  )

(defn- checksum
  [filesystem]
  (reduce-kv
    (fn [result index id]
      (if (not (number? id))
        result
        (+ result (* index id))))
    0
    filesystem))

(comment
  (checksum (defrag (parse sample0)))
;; => 1928
  )

(defn part-1 [puzzle]
  (-> puzzle
      parse
      defrag
      checksum))

(t/deftest test-part-1
  (t/is (= 1928 (part-1 sample0))))

(defn -main []
  (println (str "Day 9 Part 1: " (part-1 puzzle))))
