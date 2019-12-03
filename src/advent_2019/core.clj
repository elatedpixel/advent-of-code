(ns advent-2019.core
  (:require [clojure.java.io :as io]))

(defn read-lines [resource-file]
  (-> resource-file
      io/resource
      io/reader
      line-seq))
