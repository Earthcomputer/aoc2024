(ns aoc2024.core-test
  (:require [clojure.test :refer :all]
            [aoc2024.day1 :refer :all]
            [aoc2024.day2 :refer :all]
            [aoc2024.day3 :refer :all]
            [aoc2024.day4 :refer :all]
            [aoc2024.day5 :refer :all]
            [aoc2024.day6 :refer :all]
  ))

(defmacro make-test [day part value]
  `(deftest ~(symbol (str "day" day "-" part "-test"))
    (testing ~(str "Day " day "." part " test")
      (is (= ~value (~(symbol (str "day" day "-" part)) (slurp ~(str "testinputs/day" day ".txt")))))
    )
  )
)

(make-test 1 1 11)
(make-test 1 2 31)
(make-test 2 1 2)
(make-test 2 2 4)
(make-test 3 1 161)
(make-test 3 2 48)
(make-test 4 1 18)
(make-test 4 2 9)
(make-test 5 1 143)
(make-test 5 2 123)
(make-test 6 1 41)
(make-test 6 2 6)
