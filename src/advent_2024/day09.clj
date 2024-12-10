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
      (let [n (parse-long (str files))]
        (repeat n [id n]))
      (if (or (nil? freespace)
              (= \newline freespace))
        '()
        (let [n (parse-long (str freespace))]
          (repeat n [\. n])))))
   (range)
   (partition-all 2 input)))

(comment
  (parse sample0)
;; => ([0 2] [0 2] [\. 3] [\. 3] [\. 3] [1 3] [1 3] [1 3] [\. 3] [\. 3] [\. 3] [2 1] [\. 3] [\. 3] [\. 3] [3 3] [3 3] [3 3] [\. 1] [4 2] [4 2] [\. 1] [5 4] [5 4] [5 4] [5 4] [\. 1] [6 4] [6 4] [6 4] [6 4] [\. 1] [7 3] [7 3] [7 3] [\. 1] [8 4] [8 4] [8 4] [8 4] [9 2] [9 2])
  )

;; expects filesystem (vec "00...111...2...333.44.5555.6666.777.888899")
(defn- defrag
  "Move blocks from end of disk to fill in empty blocks."
  [filesystem]
  (loop [i  0
         j  (dec (count filesystem))
         fs (mapv first filesystem)]
    (cond
      (> i j)                    fs
      (number? (nth fs i))       (recur (inc i) j fs)
      (not (number? (nth fs j))) (recur i (dec j) fs)
      :else                      (recur (inc i)
                                        (dec j)
                                        (-> fs
                                            (assoc i (nth fs j))
                                            (assoc j (nth fs i)))))))

(comment
  (defrag (parse sample0))
;; => [0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6 \. \. \. \. \. \. \. \. \. \. \. \. \. \.]
  )

(defn- defrag-files
  "Move whole files into freespace."
  [filesystem]
  (loop [i  0
         j  (dec (count filesystem))
         id (apply max (sequence (comp (map first)
                                       (filter number?))
                                 filesystem))
         fs (vec filesystem)]
    (let [a (nth fs i)
          b (nth fs j)]
      (cond
        ;; finished
        (zero? id)                        (mapv first fs)
        ;; search is over, next id
        (> i j)                           (recur 0 (- j 1) (dec id) fs)
        ;; left index is file
        (number? (first a))               (recur (inc i) j id fs)
        ;; freespace or id we've already checked
        (or (not (number? (first b)))
            (< id (first b)))             (recur i (- j 1) id fs)
        ;; room to move entire block?
        (not-every?
          #(= \. (first %))
          (subvec fs i (+ i (second b)))) (recur (inc i) j id fs)
        ;; move file block
        :else                             (recur
                                            0
                                            (- j (second b))
                                            (dec id)
                                            (reduce
                                              (fn [fs offset]
                                                (-> fs
                                                    (assoc (+ i offset) (nth fs (- j offset)))
                                                    (assoc (- j offset) (nth fs (+ i offset)))))
                                              fs
                                              (range (second b))))))))

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

(defn part-2 [puzzle]
  (-> puzzle
      parse
      defrag-files
      checksum))

(t/deftest test-part-2
  (t/is (= 2858 (part-2 sample0))))

(defn -main []
  (println (str "Day 9 Part 1: " (part-1 puzzle)))
  (println (str "Day 9 Part 2: " (part-2 puzzle))))

(comment
  (defrag-files (parse sample0))
;; => [0 0 9 9 2 1 1 1 7 7 7 \. 4 4 \. 3 3 3 \. \. \. \. 5 5 5 5 \. 6 6 6 6 \. \. \. \. \. 8 8 8 8 \. \.]
  )
