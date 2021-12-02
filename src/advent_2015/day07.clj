(ns advent-2015.day07
  (:require [advent.core :as c]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]
            [clojure.spec.gen.alpha :as gen]))

(def input (line-seq (io/reader (io/resource "2015/day07"))))

(def varname-gen
  (gen/fmap (fn [chars] (symbol (clojure.string/lower-case (reduce str chars))))
            (gen/vector (gen/char-alpha) 1 2)))

(s/def ::varname
  (s/with-gen symbol?
    (fn [] varname-gen)))
(s/def ::val (s/alt :name ::varname
                    :value nat-int?))
(s/def ::operator #{'LSHIFT 'RSHIFT 'AND 'OR})
(s/def ::binary (s/cat :left-operand ::val
                       :operator ::operator
                       :right-operand ::val))
(s/def ::not (s/cat :not #{'NOT} :operand ::val))
(s/def ::lhs (s/alt :not ::not
                    :binary ::binary
                    :simple-value ::val))
(s/def ::rhs ::varname)
(s/def ::expression (s/cat :lhs ::lhs
                           :arrow #{'->}
                           :rhs ::rhs))

(def context (atom {}))

(defn value-by-symbol [sym]
  @(get @context sym))

(def op {'AND bit-and
         'OR bit-or
         'LSHIFT bit-shift-left
         'RSHIFT bit-shift-right})

(defn evaluate* [[kind tree-or-val]]
  (case kind
    :value tree-or-val
    :name (value-by-symbol tree-or-val)
    :simple-value (evaluate* tree-or-val)
    :not (bit-not (evaluate* (:operand tree-or-val)))
    :binary (let [l (evaluate* (:left-operand tree-or-val))
                  r (evaluate* (:right-operand tree-or-val))
                  f (op (:operator tree-or-val))]
              (f l r))))

(defn evaluate-expr! [context {:keys [lhs rhs]}]
  (swap! context assoc rhs (delay (evaluate* lhs))))

(defn parse-expression [s] (s/conform ::expression (edn/read-string (format "[%s]" s))))

(defn evaluate-lines! [expressions]
  (doseq [expression expressions]
    (evaluate-expr! context expression)))

(comment
  (do
    (evaluate-lines! (sequence (map parse-expression) input))
    (def part1 (value-by-symbol 'a)))
;
  (do
    (evaluate-lines! (sequence (map parse-expression) input))
    (swap! context assoc 'b (delay part1))
    (def part2 (value-by-symbol 'a)))
;
  )
