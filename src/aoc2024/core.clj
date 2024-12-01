(ns aoc2024.core
  (:require
    [aoc2024.util :as util]
    [aoc2024.day1 :refer :all]
    [clojure.string :as str])
)

(defn run
  "The entry point"
  [day part]
  (let [input (str/trim (util/read-input))]
    (case [day part]
      [1 1] (day1-1 input)
      [1 2] (day1-2 input)
    )
  )
)
