(ns advent-2020.day08
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def boot-parser
  (insta/parser
   "commands = command (<whitespace> command)*
    command = op <whitespace> number
    whitespace = #'\\s+'
    op = 'acc' | 'jmp' | 'nop'
    number = #'[-+][0-9]+'"))

(def operator
  {:acc (fn [state number] (-> state
                               (update :pointer inc)
                               (update :accumulator + number)))
   :jmp (fn [state number] (update state :pointer + number))
   :nop (fn [state number] (update state :pointer inc))})

(def transform-options
  {:number   read-string
   :op       keyword
   :command  vector
   :commands #(into {} (map-indexed vector) %&)})

(def input
  (slurp (io/resource "2020/day08")))

(def commands
  (insta/transform transform-options (insta/parse boot-parser input)))

(defn execute [commands]
  (loop [state    {:pointer     0
                   :accumulator 0}
         executed #{}]
    (let [{:keys [pointer accumulator] :as state'} state]
      (cond
        (executed pointer)            {:status      :looped
                                       :accumulator accumulator}
        (>= pointer (count commands)) {:status      :done
                                       :accumulator accumulator}
        :else                         (let [[op number] (commands pointer)]
                                        (recur ((operator op) state number) (conj executed pointer)))))))

;; part 1
(comment
  (execute commands))

;; part 2
(comment
  ;; there are only 224 jmp ops and 52 nop ops so let's just iterate!
  (let [experiments (filter (fn [[k [op _]]] (#{:jmp :nop} op)) commands)]
    (for [[index [op number]] experiments
          :let [output (execute (update-in commands [index 0] #(if (= :jmp %) :nop :jmp)))]
          :when (= :done (:status output))]
      [index output])))
