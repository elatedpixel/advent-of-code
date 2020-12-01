(ns advent-2016.day11
  (:require [clojure.test :as t]
            [clojure.math.combinatorics :as combo]))

(def initial-state
  {:steps     0
   :elevator  0
   :equipment [[[:promethium :generator] [:promethium :microchip]]
               [[:cobalt :generator] [:curium :generator] [:ruthenium :generator] [:plutonium :generator]]
               [[:cobalt :microchip] [:curium :microchip] [:ruthenium :microchip] [:plutonium :microchip]]
               []]})

(defn map-across-vals [f m]
  (into {} (for [[k v] m] [k (map f v)])))

(t/with-test

  (defn valid-move?
    "A row is valid if there are only generators, only microchips, pairs of generators/microchips,
     or the row is empty and the elevator is not= to the index because an empty elevator cannot move"
    [elevator index row]
    (let [[generators microchips] (->> (group-by second row)
                                       (map-across-vals first)
                                       ((juxt :generator :microchip)))]
      (if (and (= elevator index) (empty? row))
        false
        (or (and (not= elevator index)
                 (empty? row))
            (and (or (and (some? microchips) (empty? generators))
                     (and (some? generators) (empty? microchips))))
            (empty? (remove (set generators) (set microchips)))))))

  (t/are [expected elevator index row] (= expected (valid-move? elevator index row))
    true 0 1 []
    true 0 0 [[:cobalt :microchip]]
    true 0 0 [[:cobalt :generator]]
    true 0 0 [[:cobalt :microchip] [:curium :microchip]]
    true 0 0 [[:cobalt :microchip] [:cobalt :generator]]
    true 0 0 [[:cobalt :generator] [:cobalt :microchip] [:curium :generator]]
    false 0 0 []
    false 0 0 [[:cobalt :generator] [:curium :microchip]]
    false 0 0 [[:cobalt :generator] [:cobalt :microchip] [:curium :microchip]]))

(defn valid-state?
  [{:keys [elevator equipment] :as state}]
  (every? true? (map-indexed (partial valid-move? elevator) equipment)))

(t/with-test

  (defn next-moves
    [{:keys [elevator equipment] :as state}]
    ;; {:post [(do (println "next-moves" state "==>" %) true)]}
    (for [c          (concat (combo/combinations (get equipment elevator) 1)
                             (combo/combinations (get equipment elevator) 2))
          next-floor [(inc elevator) (dec elevator)]
          :when      (< -1 next-floor (count equipment))]
      (-> state
          (assoc :elevator next-floor)
          (update :equipment #(-> %
                                  (update next-floor into c)
                                  (update elevator (comp vec (partial remove (set c)))))))))

  (t/are [expected state] (= (sort-by :elevator expected)
                             (sort-by :elevator (next-moves state)))
    '({:elevator 0 :equipment [[:A] [:B :C] [:D]]}
      {:elevator 0 :equipment [[:B] [:A :C] [:D]]}
      {:elevator 0 :equipment [[:C] [:A :B] [:D]]}
      {:elevator 0 :equipment [[:A :B] [:C] [:D]]}
      {:elevator 0 :equipment [[:A :C] [:B] [:D]]}
      {:elevator 0 :equipment [[:B :C] [:A] [:D]]}
      {:elevator 2 :equipment [[] [:B :C] [:D :A]]}
      {:elevator 2 :equipment [[] [:A :C] [:D :B]]}
      {:elevator 2 :equipment [[] [:A :B] [:D :C]]}
      {:elevator 2 :equipment [[] [:C] [:D :A :B]]}
      {:elevator 2 :equipment [[] [:B] [:D :A :C]]}
      {:elevator 2 :equipment [[] [:A] [:D :B :C]]})
    {:elevator  1
     :equipment [[] [:A :B :C] [:D]]}))

(comment
  (valid-state? initial-state)
                                        ;
  (filter valid-state? (next-moves initial-state))
                                        ;
  (into [] (comp (filter valid-state?)) (next-moves initial-state))
                                        ;
  )

(defn bfs [start stop? explore]
  (loop [explored #{(:equipment start)}
         frontier (conj clojure.lang.PersistentQueue/EMPTY start)]
    (let [state    (peek frontier)
          children (filter valid-state? (explore (update state :steps inc)))]
      (if-let [stop (stop? state)]
        state
        (recur (into explored (map :equipment) children)
               (into (pop frontier) (remove (comp explored :equipment) children)))))))

(comment
  (println
   (bfs initial-state
        (fn [{:keys [steps elevator equipment]}]
          (and (empty? (equipment 0))
               (empty? (equipment 1))
               (empty? (equipment 2))))
        (memoize next-moves)))
                                        ;
  )

#_(def data
    (sequence
     parser
     (line-seq (io/reader (io/resource "2016/day11.txt")))))

(t/run-tests *ns*)

