(ns aoc2024.core-test
  (:require [clojure.test :refer :all]
            [aoc2024.day1 :refer :all]
            [aoc2024.day2 :refer :all]
            [aoc2024.day3 :refer :all]
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
