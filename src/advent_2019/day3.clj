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
    [{:keys [current-location wire-path steps total-steps]} [direction n]]
    (let [traced (reductions #(map + %1 %2) current-location
                             (repeat n (move direction)))]
      {:wire-path        (into wire-path traced)
       :current-location (last traced)
       :steps            (merge
                          (zipmap traced
                                  (take (count traced) (iterate inc total-steps)))
                          steps)
       :total-steps      (+ total-steps (count traced) -1)}))
  (is (= {:wire-path        (set '((0 103) (1 103) (2 103) (3 103)))
          :steps            {'(0 103) 0 '(1 103) 1 '(2 103) 2 '(3 103) 3}
          :current-location '(3 103)
          :total-steps      3}
         (trace {:current-location '(0 103)
                 :total-steps      0
                 :steps            {}
                 :wire-path        #{}}
                [\R 3]))))

(comment

  ;; part 1
  (apply min-key
         distance
         (clojure.set/difference
          (->> input
               (map #(:wire-path
                      (transduce (map path)
                                 (completing trace)
                                 {:current-location '(0 0)
                                  :total-steps      0
                                  :steps            {}
                                  :wire-path        #{}}
                                 %)))
               (apply clojure.set/intersection))
          #{'(0 0)}))
  ;; => (245 0)

  ;; part 2
  (let [[wire1 wire2]
        (map #(transduce (map path)
                         (completing trace)
                         {:current-location '(0 0)
                          :total-steps      0
                          :steps            {}
                          :wire-path        #{}}
                         %) input)]
    (#(+ ((:steps wire1) %) ((:steps wire2) %))
     (apply min-key
            #(+ ((:steps wire1) %) ((:steps wire2) %))
            (clojure.set/difference
             (clojure.set/intersection
              (:wire-path wire1) (:wire-path wire2))
             #{'(0 0)}))))
  ;; => 48262

  ;; formatting hax
  )

(run-tests 'advent-2019.day3)
