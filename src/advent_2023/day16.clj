(ns advent-2023.day16
  (:require
   [advent.core :as c]
   [clojure.test :as test]))

(def input (c/read-grid (c/file 2023 16)))
(def sample (c/read-grid (c/file 2023 16 ".sample")))

(defprotocol MirrorContraption
  (count-energized [this]))

(defrecord SimpleMirrors [grid light-grid]
  MirrorContraption
  (count-energized [this]
    (count (:light-grid this))))

(defn- light-bounce [dir encounter]
  (case encounter
    \. [dir]
    \\ (case dir
         [0 1]  [[1 0]]
         [1 0]  [[0 1]]
         [0 -1] [[-1 0]]
         [-1 0] [[0 -1]])
    \/ (case dir
         [0 1]  [[-1 0]]
         [1 0]  [[0 -1]]
         [0 -1] [[1 0]]
         [-1 0] [[0 1]])
    \- (case dir
         ([0 -1] [0 1]) [dir]
         ([-1 0] [1 0]) [[0 -1] [0 1]])
    \| (case dir
         ([-1 0] [1 0]) [dir]
         ([0 -1] [0 1]) [[-1 0] [1 0]])))

(defn- trace-light [grid light]
  (loop [g       {}
         seen    #{}
         explore (conj clojure.lang.PersistentQueue/EMPTY light)]
    (if (empty? explore)
      g
      (let [[coord dir :as current] (peek explore)]
        (if-let [encounter (get grid coord)]
          (recur (update g coord (fnil inc 0))
                 (conj seen current)
                 (into (pop explore)
                       (comp (map (juxt (partial mapv + coord) identity))
                             (filter (complement seen)))
                       (light-bounce dir encounter)))
          (recur g (conj seen current) (pop explore)))))))

(defn- simple-mirrors
  ([grid]
   (simple-mirrors grid [[0 0] [0 1]]))
  ([grid light]
   (->SimpleMirrors grid (trace-light grid light))))

(defn- part-1 [grid]
  (count-energized (simple-mirrors grid)))

(test/deftest test-part-1
  (test/is (= 46 (part-1 sample)))
  (test/is (= 6902 (part-1 input))))

(defn- generate-light-configurations [grid]
  (let [[y _]   (apply max-key first (keys grid))
        [_ x]   (apply max-key second (keys grid))
        corner? #{[0 0] [0 (dec x)] [(dec y) 0] [(dec y) (dec x)]}]
    (concat
     ;; corners
     [[[0 0] [0 1]]
      [[0 0] [1 0]]

      [[0 (dec x)] [1 0]]
      [[0 (dec x)] [0 -1]]

      [[(dec y) 0] [-1 0]]
      [[(dec y) 0] [0 1]]

      [[(dec y) (dec x)] [-1 0]]
      [[(dec y) (dec x)] [0 -1]]]
     ;; top row
     (for [i (range 1 (dec x))] [[0 i] [1 0]])
     ;; bottom row
     (for [i (range 1 (dec x))] [[(dec y) i] [-1 0]])
     ;; left column
     (for [i (range 1 (dec y))] [[i 0] [0 1]])
     ;; right column
     (for [i (range 1 (dec y))] [[i (dec x)] [0 -1]]))))

(defn- part-2 [grid]
  (transduce
   (map (comp count-energized
              (partial simple-mirrors grid)))
   max
   0
   (generate-light-configurations grid)))

(test/deftest test-part-2
  (test/is (= 51 (part-2 sample)))
  (test/is (= 7697 (part-2 input))))

