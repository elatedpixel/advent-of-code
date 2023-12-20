(ns advent-2023.day17
  (:require [advent.core :as c]
            #_[clojure.data.priority-map :refer [priority-map]]
            [clojure.test :as test]))

(def sample (c/read-grid (c/file 2023 17 ".sample")
                         (comp parse-long str)))

(defn- neighbors [[coord dir blocks path]]
  (let [continue-straight (if (< blocks 3) [dir] nil)]
    (mapv
     (fn [dir'] (let [coord' (mapv + coord dir')]
                  [coord' dir' (if (= dir dir') (inc blocks) 1) path]))
     (case dir
       ([0 -1] [0 1]) (into continue-straight [[-1 0] [1 0]])
       ([-1 0] [1 0]) (into continue-straight [[0 -1] [0 1]])))))

(defn- explore [grid start end]
  (loop [heat-loss []
         frontier  (conj clojure.lang.PersistentQueue/EMPTY [start [0 1] 0 #{}])]
    (if (empty? frontier)
      heat-loss
      (let [[coord dir blocks path :as current] (peek frontier)
            coords                              (filter (comp grid first) (neighbors current))]
        (cond
          (path coord)  (recur #dbg heat-loss (pop frontier))
          (= coord end) (recur #dbg (conj heat-loss (transduce (map grid) + path)) (pop frontier))
          :else         (recur heat-loss (into (pop frontier)
                                               (comp
                                                (map #(update % 3 conj coord))
                                                (remove (comp path first)))
                                               coords)))))))

(defn- part-1 [grid]
  (let [[y _] (apply max-key first (keys grid))
        [_ x] (apply max-key second (keys grid))]
    (apply min #dbg (explore grid [0 0] [y x]))))

(test/deftest test-part-1
  (test/is (= 102 (part-1 sample))))
