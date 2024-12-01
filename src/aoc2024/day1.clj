(ns aoc2024.day1
  (:require
    [aoc2024.util :as util]
    [clojure.string :as str])
)

(defn parse-input [input]
  (map
    (fn [line] (map parse-long (str/split line #" +")))
    (str/split-lines input)
  )
)

(defn day1-1 [input]
  (reduce
    +
    (apply map
      (fn [x y] (abs (- x y)))
      (map
        sort
        (util/transpose (parse-input input))
      )
    )
  )
)

(defn day1-2 [input]
  (let [[first second] (util/transpose (parse-input input))]
    (reduce
      +
      (map
        (fn [x]
          (* x (util/count-occurrences second x))
        )
        first
      )
    )
  )
)
