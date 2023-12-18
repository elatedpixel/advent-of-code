(ns advent-2023.day14
  (:require
   [advent.core :as c]
   [clojure.test :as test]))

(def input (c/read-lines (c/file 2023 14)))
(def sample (c/read-lines (c/file 2023 14 ".sample")))
(def tilted (c/read-lines (c/file 2023 14 ".tilted")))

(defn- support-load [platform]
  (loop [n      (count platform)
         result 0]
    (if (zero? n)
      result
      (let [i (- (count platform) n)]
        (recur (dec n)
               (+ result (* n (get (frequencies (nth platform i)) \O 0))))))))

(test/deftest test-support-load
  (test/is (= 136 (support-load tilted))))

(defn- tilted [column]
  (reduce
   (fn [state [i c]]
     (case c
       \. state
       \O (-> state
              (update :result + (:max state))
              (update :max dec))
       \# (assoc state :max (- (count column) i 1))))
   {:result 0 :max (count column)}
   (map-indexed vector column)))

(defn- tilted-support [platform]
  (transduce
   (map (comp :result tilted))
   +
   (apply map str platform)))

(tilted-support input)
;; => 105003


