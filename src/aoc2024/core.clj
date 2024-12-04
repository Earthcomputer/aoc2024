(ns aoc2024.core
  (:require
    [aoc2024.util :as util]
    [aoc2024.day1 :refer :all]
    [aoc2024.day2 :refer :all]
    [aoc2024.day3 :refer :all]
    [aoc2024.day4 :refer :all]
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
    )
  )
)
