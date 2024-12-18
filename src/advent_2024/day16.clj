(ns advent-2024.day16
  (:require
   [clojure.test :as t]
   [clojure.java.io :as io]))

(defrecord Reindeer [coord facing])
(defrecord Puzzle [maze ^Reindeer reindeer])
(defrecord Node [^Reindeer reindeer path])

(defn- parse [input]
  (let [maze     (into [] (map vec) (line-seq (io/reader (io/resource input))))
        reindeer (first
                  (for [y     (range (count maze))
                        x     (range (count (maze 0)))
                        :when (= \S ((maze y) x))]
                    [y x]))]
    (Puzzle. maze (Reindeer. reindeer :E))))

(defn- turns [facing]
  (case facing
    (:E :W) [:N :S]
    (:N :S) [:E :W]
    (throw (Exception. (format "Unexpected value to turns(%s)." facing)))))

(defn- vec+ [a b] (mapv + a b))

(defn- moves [^Node {:keys [reindeer path] :as current}]
  (let [{:keys [coord facing] :as reindeer} reindeer]
    (cons
     (-> current
         (update :path (fnil inc 0))
         (update-in [:reindeer :coord]
                    (case facing
                      :E (partial vec+ [0 1])
                      :W (partial vec+ [0 -1])
                      :N (partial vec+ [-1 0])
                      :S (partial vec+ [1 0])
                      (throw (Exception. (format "Unexpected value to moves(%s)." facing))))))
     (map (fn [facing'] (Node. (Reindeer. coord facing') (+ path 1000))) (turns facing)))))

(defn- search [^Puzzle {:keys [reindeer maze]}]
  (let [start (Node. reindeer 0)]
    (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY start)
           explored #{reindeer}
           scores   []]
      (if (empty? frontier) scores
          (let [current (peek frontier)]
            (recur
              (into (pop frontier)
                    (remove #(or (explored (:reindeer %))
                                 (#{\#} (get-in maze (get-in % [:reindeer :coord]))))
                            (moves current)))
              (conj explored (:reindeer current))
              (if (= \E (get-in maze (get-in current [:reindeer :coord])))
                (conj scores (get current :path))
                scores)))))))

(def ^:private puzzle (parse "2024/day16.txt"))
(def ^:private sample0 (parse "2024/day16.sample0"))

(comment
  (apply min (search puzzle))
;
  )
