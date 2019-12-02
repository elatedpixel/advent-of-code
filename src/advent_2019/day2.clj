(ns advent-2019.day2
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

(def input
  (->> "2019/day2.txt"
       io/resource
       io/reader
       line-seq
       first
       (format "(%s)")
       read-string))

(defn input->map [d]
  (zipmap (range) d))

(def opcode
  {1 +
   2 *})

(defn sort-by-keys [m]
  (map m (sort (keys m))))

(t/with-test

  (defn computer [intcode]
    (loop [i 0
           program intcode]
      (let [[op a b r] (nth (partition-all 4 4 (sort-by-keys program)) i)]
        (if (= 99 op) program
            (recur (inc i) (assoc program r ((opcode op) (program a) (program b))))))))

  (t/is (= [2 0 0 0 99] (sort-by-keys (computer (input->map [1 0 0 0 99])))))
  (t/is (= [2 3 0 6 99] (sort-by-keys (computer (input->map [2 3 0 3 99])))))
  (t/is (= [2 4 4 5 99 9801] (sort-by-keys (computer (input->map [2 4 4 5 99 0])))))
  (t/is (= [30 1 1 4 2 5 6 0 99] (sort-by-keys (computer (input->map [1 1 1 4 99 5 6 0 99]))))))

(t/run-tests 'advent-2019.day2)

(defn -main []
  (time (println ((computer (-> input
                                input->map
                                (assoc 1 12)
                                (assoc 2 2))) 0)))
  (time (println (ffirst (filter #(= 19690720 ((second %) 0))
                          (for [a (range 100) b (range 100)]
                            [(+ b (* 100 a))
                             (computer (-> input
                                           input->map
                                           (assoc 1 a)
                                           (assoc 2 b)))]))))))
