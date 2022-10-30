(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(defn load-input [year day & [{:keys [test]}]]
  (cond-> (format "%d/day%02d" year day)
    (some? test) (str "-test")
    :always ((comp line-seq io/reader io/resource))))

(defn filter-key-by-val [pred m]
  (for [[k v] m :when (pred v)] k))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra [g start]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (g v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn dfs [start stop? explore]
  (loop [explored (set)
         frontier (conj [] start)]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:explored       explored
                                     :explored-count (count explored)
                                     :frontier       frontier}))
      (let [state    (peek frontier)
            children (explore (update state :steps inc))]
        (if (stop? state)
          state
          (recur (conj explored (:equipment state))
                 (into (pop frontier) (remove (comp explored :equipment) children))))))))

(defn bfs [start stop? explore]
  (loop [explored #{(:equipment start)}
         frontier (conj clojure.lang.PersistentQueue/EMPTY start)]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:explored       explored
                                     :explored-count (count explored)
                                     :frontier       frontier}))
      (let [state    (peek frontier)
            children (explore (update state :steps inc))]
        (if (stop? state)
          state
          (recur (into explored (map :equipment) children)
                 (into (pop frontier) (remove (comp explored :equipment) children))))))))

(defn string->sexpression
  [s] (read-string (str "(" s ")")))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn mode [coll]
  (let [freqs (frequencies coll)
        occurrences (group-by val freqs)
        modes (last (sort occurrences))
        modes (->> modes
                   val
                   (map key))]
    modes))

(defn key-by [f selector] (fn [m] (key (apply f selector m))))

(defprotocol ContextMachine
  (learn! [this learning-sequence learn-fn])
  (resolve* [this context-key]))

(defrecord DelayMachine [context]
  ContextMachine
  (learn! [this learning-sequence learn-fn]
    (doseq [expression learning-sequence]
            (learn-fn expression)))
  (resolve* [this context-key]
    @(get @(:context this) context-key)))
