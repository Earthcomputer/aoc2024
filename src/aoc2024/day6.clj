(ns aoc2024.day6
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions [[-1 0] [0 1] [1 0] [0 -1]])

(defn- simulate [grid]
  (let [[x y] (util/find-in-grid grid #(= \^ %))]
    (loop [
        pos [y x]
        dir 0
        visited #{pos}
        cycle-detector #{}
      ]
      (if
        (contains? cycle-detector [pos dir])
        nil
        (let [next-pos (mapv + pos (get directions dir))]
          (case (get-in grid next-pos)
            nil (count visited)
            \# (recur pos (mod (inc dir) 4) visited (conj cycle-detector [pos dir]))
            (recur next-pos dir (conj visited next-pos) (conj cycle-detector [pos dir]))
          )
        )
      )
    )
  )
)

(defn day6-1 [input]
  (let [grid (vec (str/split-lines input))]
    (simulate grid)
  )
)

(defn day6-2 [input]
  (let [grid (vec (str/split-lines input))]
    (util/count-matches
      (fn [[x y]]
        (and
          (= \. (get-in grid [y x]))
          (nil? (simulate (util/set-in-grid grid [y x] \#)))
        )
      )
      (util/grid-coords grid)
    )
  )
)
