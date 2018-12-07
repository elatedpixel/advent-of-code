(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

(deftest test-divide-and-sum
  (are [result input] (= result (divide-and-sum input))
       9 [[5 9 2 8]
          [9 4 7 3]
          [3 8 6 5]]))

(run-tests)
