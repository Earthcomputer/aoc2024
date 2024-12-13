(ns aoc2024.core
  (:require
    [aoc2024.util :as util]
    [aoc2024.day1 :refer :all]
    [aoc2024.day2 :refer :all]
    [aoc2024.day3 :refer :all]
    [aoc2024.day4 :refer :all]
    [aoc2024.day5 :refer :all]
    [aoc2024.day6 :refer :all]
    [aoc2024.day7 :refer :all]
    [aoc2024.day8 :refer :all]
    [aoc2024.day9 :refer :all]
    [aoc2024.day10 :refer :all]
    [aoc2024.day11 :refer :all]
    [aoc2024.day12 :refer :all]
    [aoc2024.day13 :refer :all]
    [clojure.string :as str])
)

(defn run
  "The entry point"
  [day part]
  (let [input (str/trim (util/read-input))]
    (case [day part]
      [1 1] (day1-1 input)
      [1 2] (day1-2 input)
      [2 1] (day2-1 input)
      [2 2] (day2-2 input)
      [3 1] (day3-1 input)
      [3 2] (day3-2 input)
      [4 1] (day4-1 input)
      [4 2] (day4-2 input)
      [5 1] (day5-1 input)
      [5 2] (day5-2 input)
      [6 1] (day6-1 input)
      [6 2] (day6-2 input)
      [7 1] (day7-1 input)
      [7 2] (day7-2 input)
      [8 1] (day8-1 input)
      [8 2] (day8-2 input)
      [9 1] (day9-1 input)
      [9 2] (day9-2 input)
      [10 1] (day10-1 input)
      [10 2] (day10-2 input)
      [11 1] (day11-1 input)
      [11 2] (day11-2 input)
      [12 1] (day12-1 input)
      [12 2] (day12-2 input)
      [13 1] (day13-1 input)
      [13 2] (day13-2 input)
    )
  )
)
