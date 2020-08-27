(ns advent-2016.day10
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]
            [clojure.spec.gen.alpha :as gen]
            [clojure.java.io :as io]))

(s/def ::actor #{'bot 'output})
(s/def ::hilo #{'low 'high})

(s/def ::deliver-value
  (s/cat :_ #{'value}
         :value int?
         :_ #{'goes}
         :_ #{'to}
         :to-actor ::actor
         :id int?))

(s/def ::bot-give
  (s/cat :_ #{'gives 'and}
         :high-or-low ::hilo
         :_ #{'to}
         :to-actor ::actor
         :id int?))

(s/def ::bot-distribution
  (s/cat :from-actor ::actor
         :from-id int?
         :low ::bot-give
         :high ::bot-give))

(s/def ::bot-instruction
  (s/alt :input ::deliver-value
         :output ::bot-distribution))

(def parser
  (comp
   (map #(str "[" % "]"))
   (map edn/read-string)
   (map #(s/conform ::bot-instruction %))))

(def data
  (sequence
   parser
   (line-seq (io/reader (io/resource "2016/day10.txt")))))

(def context (atom {}))

(defn evaluate [path]
  @(get-in @context path))

(defmulti process-instruction first)
(defmethod process-instruction :input [_ {:keys [value to-actor id]}]
  (update-in context [to-actor id] conj (delay value)))
(defmethod process-instruction :output [_ {:keys [from-actor from-id low high]}]
  (let [[l h] (get-in @context [from-actor from-id])]
    (-> context
        (assoc-in [(:to-actor low) (:id low)] l)
        (assoc-in [(:to-actor high) (:id high)] h))))

(comment
  (doseq [instruction (take 2 data)]
    (println instruction)
    (process-instruction instruction))

  (first data)
;; => [:output
;;     {:from-actor bot,
;;      :from-id 49,
;;      :low {:_ to, :high-or-low low, :to-actor bot, :to-actor-value 118},
;;      :high {:_ to, :high-or-low high, :to-actor bot, :to-actor-value 182}}]
                                        ;
  )

(comment
  (def rule-regex #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")
  (def value-regex #"value (\d+) goes to bot (\d+)")

  (or (re-matches rule-regex "bot 49 gives low to bot 118 and high to bot 182")
      (re-matches value-regex "bot 49 gives low to bot 118 and high to bot 182"))

  "bot 49 gives low to bot 118 and high to bot 182"

  "bot 195 gives low to output 4 and high to bot 130"

  "value 61 goes to bot 49"
  ;; 
  )
