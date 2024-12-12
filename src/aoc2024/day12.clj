(ns aoc2024.day12
  (:require [clojure.string :as str]))

(def ^:private directions (vec (partition 2 1 [[-1 0] [0 -1] [1 0] [0 1] [-1 0]])))

(defn- get-perimeter-area [grid x y visited part2]
  (let [c (get-in grid [y x])]
    (reduce
      (fn [[perimeter area visited] [[dx dy] [dx1 dy1]]]
        (let [x1 (+ x dx) y1 (+ y dy)]
          (cond
            (not= c (get-in grid [y1 x1])) (if
              (or (not part2) (not= c (get-in grid [(+ y dy1) (+ x dx1)])) (= c (get-in grid [(+ y1 dy1) (+ x1 dx1)])))
              [(inc perimeter) area visited]
              [perimeter area visited]
            )
            (contains? visited [x1 y1]) [perimeter area visited]
            :else (let [[perimeter1 area1 visited] (get-perimeter-area grid x1 y1 visited part2)]
              [(+ perimeter perimeter1) (+ area area1) visited]
            )
          )
        )
      )
      [0 1 (conj visited [x y])]
      directions
    )
  )
)

(defn- run [input part2]
  (let [grid (vec (str/split-lines input))]
    (first
      (reduce
        (fn [[result visited] y]
          (reduce
            (fn [[result visited] x]
              (if (contains? visited [x y])
                [result visited]
                (let [[perimeter area visited] (get-perimeter-area grid x y visited part2)]
                  [(+ result (* perimeter area)) visited]
                )
              )
            )
            [result visited]
            (range (count (get grid y)))
          )
        )
        [0 #{}]
        (range (count grid))
      )
    )
  )
)

(defn day12-1 [input]
  (run input false)
)

(defn day12-2 [input]
  (run input true)
)
