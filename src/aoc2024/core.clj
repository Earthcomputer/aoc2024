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
    [aoc2024.day14 :refer :all]
    [aoc2024.day15 :refer :all]
    [aoc2024.day16 :refer :all]
    [aoc2024.day17 :refer :all]
    [aoc2024.day18 :refer :all]
    [aoc2024.day19 :refer :all]
    [aoc2024.day20 :refer :all]
    [aoc2024.day21 :refer :all]
    [aoc2024.day22 :refer :all]
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
      [14 1] (day14-1 input)
      [14 2] (day14-2 input)
      [15 1] (day15-1 input)
      [15 2] (day15-2 input)
      [16 1] (day16-1 input)
      [16 2] (day16-2 input)
      [17 1] (day17-1 input)
      [17 2] (day17-2 input)
      [18 1] (day18-1 input)
      [18 2] (day18-2 input)
      [19 1] (day19-1 input)
      [19 2] (day19-2 input)
      [20 1] (day20-1 input)
      [20 2] (day20-2 input)
      [21 1] (day21-1 input)
      [21 2] (day21-2 input)
      [22 1] (day22-1 input)
      [22 2] (day22-2 input)
    )
  )
)
