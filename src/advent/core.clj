(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]))

(defn load-input [year day]
  (line-seq (io/reader (io/resource (format "%d/day%02d" year day)))))

(defn string->sexpression
  [s] (read-string (str "(" s ")")))

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
