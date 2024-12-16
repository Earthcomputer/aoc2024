(ns aoc2024.day4
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions [
  [-1 -1]
  [0 -1]
  [1 -1]
  [-1 0]
  [1 0]
  [-1 1]
  [0 1]
  [1 1]
])

(defn- run [input count-matches-at-func]
  (let [wordsearch (str/split-lines input)]
    (reduce +
      (map
        (fn [[x y]] (count-matches-at-func wordsearch x y))
        (util/grid-coords wordsearch)
      )
    )
  )
)

(defn day4-1 [input]
  (run input
    (fn [wordsearch x y]
      (util/count-matches
        (fn [[dx dy]]
          (and
            (= \X (get-in wordsearch [y x]))
            (= \M (get-in wordsearch [(+ y dy) (+ x dx)]))
            (= \A (get-in wordsearch [(+ y (* dy 2)) (+ x (* dx 2))]))
            (= \S (get-in wordsearch [(+ y (* dy 3)) (+ x (* dx 3))]))
          )
        )
        directions
      )
    )
  )
)

(defn day4-2 [input]
  (run input
    (fn [wordsearch x y]
      (if
        (and
          (= \A (get-in wordsearch [y x]))
          (or
            (and
              (= \M (get-in wordsearch [(- y 1) (- x 1)]))
              (= \S (get-in wordsearch [(+ y 1) (+ x 1)]))
            )
            (and
              (= \S (get-in wordsearch [(- y 1) (- x 1)]))
              (= \M (get-in wordsearch [(+ y 1) (+ x 1)]))
            )
          )
          (or
            (and
              (= \M (get-in wordsearch [(- y 1) (+ x 1)]))
              (= \S (get-in wordsearch [(+ y 1) (- x 1)]))
            )
            (and
              (= \S (get-in wordsearch [(- y 1) (+ x 1)]))
              (= \M (get-in wordsearch [(+ y 1) (- x 1)]))
            )
          )
        )
        1 0
      )
    )
  )
)
