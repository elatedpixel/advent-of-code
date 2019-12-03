(ns advent-2019.day3
  (:require [advent-2019.core :refer [read-lines]]
            [clojure.test :refer [is with-test run-tests]]))

(def input
  (map #(clojure.string/split % #",")
       (read-lines "2019/day3.txt")))

(defn distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(with-test
  (def path
    (juxt first #(Integer/parseInt (apply str (rest %)))))
  (is (= [\R 103] (path "R103"))))

(def move
  {\L [-1 0]
   \R [1 0]
   \D [0 -1]
   \U [0 1]})

(with-test

  (defn trace
    [wire-path [direction n]]
    (->> direction
         move
         (repeat n)
         (reductions #(map + %1 %2)
                     (last wire-path))
         rest
         (concat wire-path)))

  (is (= '((0 103) (1 103) (2 103) (3 103))
         (trace '((0 103)) [\R 3]))))

(def tracer (partial transduce (map path) (completing trace)))

(comment
  (time
   (let [paths         (map #(rest (tracer '((0 0)) %)) input)
         intersections (apply clojure.set/intersection (map set paths))]

     ;; part 1
     (println (apply min-key distance intersections))

     ;; part 2
     (println (+ (count paths)          ; because we skip the origin in all paths
                 (->> intersections
                      (map (fn [v] (apply + (map #(.indexOf % v) paths))))
                      (apply min))))
     ))

  ;; (245 0)
  ;; 48262
  ;; "Elapsed time: 14871.189122 msecs"

  )

(run-tests 'advent-2019.day3)
