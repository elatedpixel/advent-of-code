(ns advent-2016.day11
  (:require [clojure.test :as t]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.math.combinatorics :as combo]))

(def initial-state
  {:steps     0
   :elevator  0
   :equipment [#{[:promethium :generator] [:promethium :microchip]}
               #{[:plutonium :generator] [:curium :generator] [:ruthenium :generator] [:cobalt :generator]}
               #{[:plutonium :microchip] [:curium :microchip] [:ruthenium :microchip] [:cobalt :microchip]}
               #{}]})

(def advanced-state
  (update-in initial-state [:equipment 0] into
          #{[:elerium :generator] [:elerium :microchip]
            [:dilithium :generator] [:dilithium :microchip]}) )

(def toy-state
  {:steps     0
   :elevator  0
   :equipment [#{[:hydrogen :microchip] [:lithium :microchip]}
               #{[:hydrogen :generator]}
               #{[:lithium :generator]}
               #{}]})

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
      (if (empty? row)
        (not= elevator index)
        (or (empty? generators)
            (empty? microchips)
            (empty? (remove (set generators) (set microchips)))))))

  (t/are [expected elevator index row] (= expected (valid-move? elevator index row))
    true 0 1 #{}
    true 0 0 #{[:cobalt :microchip]}
    true 0 0 #{[:cobalt :generator]}
    true 0 0 #{[:cobalt :microchip] [:curium :microchip]}
    true 0 0 #{[:cobalt :microchip] [:cobalt :generator]}
    true 0 0 #{[:cobalt :generator] [:cobalt :microchip] [:curium :generator]}
    false 0 0 #{}
    false 0 0 #{[:cobalt :generator] [:curium :microchip]}
    false 0 0 #{[:cobalt :generator] [:cobalt :microchip] [:curium :microchip]}))

(defn valid-state?
  [{:keys [elevator equipment] :as state}]
  (every? true? (map-indexed (partial valid-move? elevator) equipment)))

(t/with-test

  (defn next-moves
    [{:keys [elevator equipment] :as state}]
    ;; {:post [(do (println "next-moves" state "==>" %) true)]}
    (for [c          (concat (combo/combinations (vec (get equipment elevator)) 1)
                             (combo/combinations (vec (get equipment elevator)) 2))
          next-floor [(inc elevator) (dec elevator)]
          :when      (<= 0 next-floor (count equipment))]
      (-> state
          (assoc :elevator next-floor)
          (update :equipment #(-> %
                                  (update next-floor into c)
                                  (update elevator (partial apply disj) (set c)))))))

  (t/are [expected state] (= (sort-by :elevator expected)
                             (sort-by :elevator (next-moves state)))
    '({:elevator 0 :equipment [#{:A} #{:B :C} #{:D}]}
      {:elevator 0 :equipment [#{:B} #{:A :C} #{:D}]}
      {:elevator 0 :equipment [#{:C} #{:A :B} #{:D}]}
      {:elevator 0 :equipment [#{:A :B} #{:C} #{:D}]}
      {:elevator 0 :equipment [#{:A :C} #{:B} #{:D}]}
      {:elevator 0 :equipment [#{:B :C} #{:A} #{:D}]}
      {:elevator 2 :equipment [#{} #{:B :C} #{:D :A}]}
      {:elevator 2 :equipment [#{} #{:A :C} #{:D :B}]}
      {:elevator 2 :equipment [#{} #{:A :B} #{:D :C}]}
      {:elevator 2 :equipment [#{} #{:C} #{:D :A :B}]}
      {:elevator 2 :equipment [#{} #{:B} #{:D :A :C}]}
      {:elevator 2 :equipment [#{} #{:A} #{:D :B :C}]})
    {:elevator  1
     :equipment [#{} #{:A :B :C} #{:D}]}))

(def goal? #(every? empty? [((:equipment %) 0)
                            ((:equipment %) 1)
                            ((:equipment %) 2)]))

(defn a*
  "https://en.wikipedia.org/wiki/A*_search_algorithm"
  [start end? heuristic explore]
  (loop [explored #{(dissoc start :steps)}
         frontier (priority-map start 0)]
    (if (empty? frontier) :no-solution
        (let [[state priority] (peek frontier)
              neighbors        (filter (comp (complement explored) #(dissoc % :steps)) (explore state))
              heuristics       (map heuristic neighbors)]
          (if (end? state) state
              (recur (into explored (map #(dissoc % :steps) neighbors))
                     (into (pop frontier) (map vector neighbors heuristics))))))))

(comment
  (time
   (println
    (a* advanced-state
        goal?
        #(let [counts (map count (:equipment %))]
            (reduce + (map * counts [3/2 1 1/2 0])))
        #(filter valid-state? (next-moves (update % :steps inc))))))
                                        ;
  )

(defn bfs [start stop? explore]
  (loop [explored #{(:equipment start)}
         frontier (conj clojure.lang.PersistentQueue/EMPTY start)]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:explored-count (count explored)}))
      (let [state    (peek frontier)
            children (filter valid-state? (explore (update state :steps inc)))]
        (if (stop? state)
          state
          (recur (into explored (map :equipment) children)
                 (into (pop frontier) (remove (comp explored :equipment) children))))))))

;; part 1
(comment
  (time
   (println
    (bfs toy-state
         (fn [{:keys [equipment]}]
           (and (empty? (equipment 0))
                (empty? (equipment 1))
                (empty? (equipment 2))))
         next-moves)))
                                        ;
  )

;; part 2
(comment
  (time
   (println
    (bfs initial-state
         (fn [{:keys [equipment]}]
           (and (empty? (equipment 0))
                (empty? (equipment 1))
                (empty? (equipment 2))))
         next-moves)))
                                        ;
  )

(t/run-tests *ns*)
