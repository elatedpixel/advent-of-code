(ns advent-2015.day07
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]
            [clojure.spec.gen.alpha :as gen]))

(def input (line-seq (io/reader (io/resource "2015/day07"))))

()

(def NOT complement)
(def OR bit-or)
(def AND bit-and)
(def LSHIFT bit-shift-left)
(def RSHIFT bit-shift-right)

(def state (atom {}))
(defmacro parse
  ([a _ wire] (list 'assoc state wire 'delay a))
  ([unary-op a _ wire] (list 'assoc state wire 'delay unary-op a))
  ([a binary-op b _ wire] (list 'assoc state wire 'delay binary-op a b)))

(macroexpand-1 '(parse NOT a -> b))
