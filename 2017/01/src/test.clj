(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

(deftest test-sum-matching-pairs
  (testing "sum-matching-pairs"
    (testing "works with even, matching pairs"
      (is (= 4 (sum-matching-pairs "1111"))))))

(run-tests)
