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
  (->> input
    (parse-input)
    (util/transpose)
    (map sort)
    (apply map #(abs (- %1 %2)))
    (reduce +)
  )
)

(defn day1-2 [input]
  (let [[first second] (util/transpose (parse-input input))]
    (->> first
      (map (fn [x] (* x (util/count-occurrences second x))))
      (reduce +)
    )
  )
)
