(ns aoc2024.day25
  (:require [aoc2024.util :as util]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as str]))

(defn- parse-input [input]
  (let [lines (vec (str/split-lines input))]
    (assert (= 7 (rem (count lines) 8)))
    (mapv vec (partition 8 (conj lines "")))
  )
)

(defn day25-1 [input]
  (util/count-matches
    (fn [[first second]]
      (not-any?
        (fn [[x y]] (= \# (get-in first [y x]) (get-in second [y x])))
        (util/grid-coords first)
      )
    )
    (combinatorics/combinations (parse-input input) 2)
  )
)
