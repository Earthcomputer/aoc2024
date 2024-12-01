(ns aoc2024.core-test
  (:require [clojure.test :refer :all]
            [aoc2024.day1 :refer :all]))

(deftest day1-1-test
  (testing "Day 1.1 test"
    (is (= 11 (day1-1 (slurp "testinputs/day1.txt"))))))

(deftest day1-2-test
  (testing "Day 1.2 test"
    (is (= 31 (day1-2 (slurp "testinputs/day1.txt"))))))
