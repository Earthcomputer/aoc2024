(ns aoc2024.day6
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions [[-1 0] [0 1] [1 0] [0 -1]])

(defn- simulate [grid]
  (loop [
      pos [(util/index-of-fn grid #(str/includes? % "^")) (str/index-of (first (filter #(str/includes? % "^") grid)) "^")]
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

(defn day6-1 [input]
  (let [grid (vec (str/split-lines input))]
    (simulate grid)
  )
)

(defn- set-wall [grid pos]
  (let [[y x] pos]
    (vec (map-indexed (fn [yi line] (if (= yi y) (str (subs line 0 x) \# (subs line (inc x))) line)) grid))
  )
)

(defn day6-2 [input]
  (let [grid (vec (str/split-lines input))]
    (reduce +
      (map
        (fn [y]
          (util/count-matches
            (fn [x]
              (and
                (= \. (get-in grid [y x]))
                (nil? (simulate (set-wall grid [y x])))
              )
            )
            (range (count (get grid y)))
          )
        )
        (range (count grid))
      )
    )
  )
)
