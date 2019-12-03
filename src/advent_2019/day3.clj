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
    [{:keys [current-location wire-path]} [direction n]]
    (let [traced (reductions #(map + %1 %2) current-location
                             (repeat n (move direction)))]
      {:wire-path        (into wire-path traced)
       :current-location (last traced)}))
  (is (= {:wire-path        #{'(0 103) '(1 103) '(2 103) '(3 103)}
          :current-location '(3 103)}
         (trace {:current-location '(0 103)
                 :wire-path        #{}}
                [\R 3]))))

(comment

  ;; part 1
  (apply min-key
         distance
         (clojure.set/difference
          (apply clojure.set/intersection
                 (map #(:wire-path
                        (transduce (map path)
                                   (completing trace)
                                   {:current-location '(0 0)
                                    :wire-path        #{}}
                                   %))
                      input))
          #{'(0 0)}))
  )

(run-tests 'advent-2019.day3)
