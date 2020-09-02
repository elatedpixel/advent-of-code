(ns advent-2016.day10
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]
            [clojure.spec.gen.alpha :as gen]
            [clojure.core.async :refer [go >!! >! <! <!! chan poll!]]
            [clojure.java.io :as io]))

(def rule-regex #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")
(def value-regex #"value (\d+) goes to bot (\d+)")

(defn make-instruction
  ([match? value-string id-string]
   {:type  :value
    :value (Integer/parseInt value-string)
    :path  [:bot (Integer/parseInt id-string)]})
  ([match? from-id-string low-type-string low-id-string high-type-string high-id-string]
   {:type      :rule
    :from-path [:bot (Integer/parseInt from-id-string)]
    :low-path  [(keyword low-type-string) (Integer/parseInt low-id-string)]
    :high-path [(keyword high-type-string) (Integer/parseInt high-id-string)]}))

(def parser
  (comp
   (map (some-fn
         (partial re-matches rule-regex)
         (partial re-matches value-regex)))
   (map (partial apply make-instruction))))

(def data
  (sequence
   parser
   (line-seq (io/reader (io/resource "2016/day10.txt")))))

(let [context (atom (transduce
                     (comp
                      (mapcat (some-fn (comp :path vector) (juxt :from-path :low-path :high-path)))
                      (distinct))
                     (completing (fn [m path] (assoc-in m path (chan 10))))
                     {}
                     data))
      comparisons (atom [])]

  (defn channel-for-path [path]
    (get-in @context path))

  (defmulti build-operation :type)

  (defmethod build-operation :value [{:keys [path value]}]
    (>!! (channel-for-path path) value))

  (defmethod build-operation :rule [{:keys [from-path low-path high-path]}]
    (go
      (let [from-chan (channel-for-path from-path)
            a         (<! from-chan)
            b         (<! from-chan)
            low       (min a b)
            high      (max a b)]
        (do
          (swap! comparisons conj [from-path low high])
          (>! (get-in @context low-path) low)
          (>! (get-in @context high-path) high)))))

  (defn get-context [] @context)
  (defn get-comparisons [] @comparisons)
                                        ; end of context closure
  )

(comment
  (doseq [instruction data]
    (build-operation instruction))

  ;; part 1
  (filter #(= (list 17 61) (rest %)) (get-comparisons))
  ;; => ([[:bot 181] 17 61])

  ;; part 2
  (reduce * (map poll! (vals (select-keys (:output (get-context)) [0 1 2]))))
  ;; => 12567

                                        ;
  )

