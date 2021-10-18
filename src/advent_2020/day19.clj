(ns advent-2020.day19
  (:require [clojure.test :as t]
            [clojure.java.io :as io]))

;; okay, my thinking here is to parse all of the rules into a hash-map, and then
;; generate a tree and search using depth-first search. the top of the tree will
;; be defined by the index selected for the first rule. because the rules are
;; static, the tree can be computed once and used for each element in the list
;; of messages.
;;
;; Example rules:
;; 0: 4 1 5
;; 1: 2 3 | 3 2
;; 2: 4 4 | 5 5
;; 3: 4 5 | 5 4
;; 4: "a"
;; 5: "b"
;;
;; Example messages:
;; ababbb
;; bababa
;; abbbab
;; aaabbb
;; aaaabbb

(def input (->> "2020/day19"
               (io/resource)
               (io/reader)
               (line-seq)
               (split-with #(not= % ""))
               ))

(defn dfs [graph explore stop? index]
  (loop [explored #{index}
         frontier [index]]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:expored  expored
                                     :frontier frontier}))
      (let [node     (graph index)
            children (explore rule)]
        (if (stop? node)
          node
          (recur (into explored children)
                 (into (pop frontier) (remove explored children))))
        ))))

(defn bfs [start stop? explore]
  (loop [explored #{(:equipment start)}
         frontier (conj clojure.lang.PersistentQueue/EMPTY start)]
    (if (empty? frontier)
      (throw (ex-info "no solution" {:explored       explored
                                     :explored-count (count explored)
                                     :frontier       frontier}))
      (let [state    (peek frontier)
            children (filter valid-state? (explore (update state :steps inc)))]
        (if (stop? state)
          state
          (recur (into explored (map :equipment) children)
                 (into (pop frontier) (remove (comp explored :equipment) children))))))))

;; if rule at index is character, return function to match character
;; if rule is vector then
(defn ^{:private true} rule-builder [[rule & rules] start-index]
  (lazy-seq
    (cond
      (string? rule) (cons rule (rule-builder rules start-index))
    (cons (if ))))

(defn valid? [rules start-index s]
  (let [rule-seq (rules-builder rules start-index)]
    (every-pred  ))
  )

(t/deftest test-rules

  (t/testing "simple rules"
    (let [rules {0 \a}]

      (t/is (true? (valid? rules 0 "a")))

      (t/is (false? (valid? rules 0 "b")))))

  (t/testing "reference rules"
    (let [rules {0 [1 2]
                 1 [2 2]
                 2 "a"}]
      (t/is (true? (valid? rules 0 "aaa"))))))
