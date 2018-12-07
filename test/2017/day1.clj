(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

(deftest test-sum-matching-pairs
  (testing "sum-matching-pairs"
    (testing "works with even, matching pairs"
      (is (= 4 (sum-matching-pairs "1111"))))
    (testing "works with uneven pair - first and last matches"
      (is (= 5 (sum-matching-pairs "2124332"))))))

(deftest test-sum-matching-circle-pairs
  (testing "sum-matching-circle-pairs"
    (testing "works with even number"
      (is (= 6 (sum-matching-circle-pairs "1212"))))))

(run-tests)
