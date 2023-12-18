(ns advent.core
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn file
  ([year day] (format "%d/day%02d" year day))
  ([year day suffix] (str (file year day) suffix)))

(def read-file (comp slurp io/resource))

(defn read-lines [file]
  (str/split-lines (read-file file)))

(defn read-paragraphs [file]
  (str/split (read-file file) #"\n\n"))

(defn read-grid [file]
  (into {} (for [[i row] (map-indexed vector (read-lines file))
                 [j col] (map-indexed vector row)]
             [[i j] col])))

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

(defn distance [[y1 x1] [y2 x2]]
  (let [dy (- y1 y2)
        dx (- x1 x2)]
    (Math/sqrt (+ (* dy dy) (* dx dx)))))

(comment (distance [1 3] [7 3]))

(defn dfs [start stop? explore]
  (loop [explored #{}
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
;; => #'advent.core/dfs;; => #'advent.core/dfs
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
