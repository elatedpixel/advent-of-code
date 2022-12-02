(ns advent-2015.day19
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.priority-map :as pm]))

(defn indexes
  ([s ss] (indexes s ss 0))
  ([s ss i]
   (lazy-seq
    (when-let [j (str/index-of s ss i)]
      (cons j (indexes s ss (inc j)))))))

(defn- generate [replacements molecule]
  (into #{}
        (for [k (keys replacements)
              r (replacements k)
              i (indexes molecule k)]
          (str (subs molecule 0 i) r (subs molecule (+ i (count k)))))))

(test/deftest test-generate

  (let [replacements {"H" ["HO" "OH"] "O" ["HH"]}]

    (test/testing "generates the right molecules in a single step"
      (test/is (= #{"HOOH" "OHOH" "HOHO" "HHHH"}
                  (generate replacements "HOH")))
      (test/is (= 7 (count (generate replacements "HOHOHO")))))

    (test/testing "ignores surrounding characters"
      (test/is (= #{"0020"} (generate {"H" ["00"]} "H20")))))

;
  )

(let [[rules molecule] (str/split (slurp (io/resource "2015/day19")) #"\n\n")
      replacements (reduce
                    (fn [m s] (let [[k v] (str/split s #" => ")]
                                (update m k (fnil conj []) v)))
                    {}
                    (str/split-lines rules))]
  (count (generate replacements molecule)))
;; => 518

;; brute-force approach here using BFS, search space is too big
(defn recipe [molecule replacements medicine]
  (loop [explored #{}
         frontier (conj clojure.lang.PersistentQueue/EMPTY (list 0 molecule))]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:explored       explored
                                     :explored-count (count explored)
                                     :frontier       frontier}))
      (let [current    (peek frontier)]
        (cond
          (or (> (first current) 10)
              (= (second current) medicine))
          current

          (some? (explored (second current)))
          (recur explored (pop frontier))

          :else
          (recur (conj explored (second current))
                 (into (pop frontier)
                       (map list (repeat (inc (first current)))
                            (remove explored (generate replacements (second current)))))))))))

#_(defn map-invert [m]
  (reduce-kv (fn [m k v] (reduce (fn [m x] (update m x (fnil conj []) k)) m v))
             {}
             m))

;; priority queue greedy approach also runs out of memory!
(defn priority-recipe [molecule replacements medicine]
  (loop [q (pm/priority-map-keyfn-by first > molecule [0 0])]
    (let [[molecule [_ steps]] (peek q)]
      (cond
        (= medicine molecule) steps

        :else (recur (into (pop q)
                           (mapv (fn [x] [x [(count x) (inc steps)]])
                                 (generate replacements molecule))))))))

(comment
  (let [[rules molecule] (str/split (slurp (io/resource "2015/day19")) #"\n\n")
       replacements (reduce
                     (fn [m s] (let [[k v] (str/split s #" => ")]
                                 (update m k (fnil conj []) v)))
                     {}
                     (str/split-lines rules))]
   (println (priority-recipe "e" replacements molecule))))
