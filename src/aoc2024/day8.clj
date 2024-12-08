(ns aoc2024.day8
  (:require [aoc2024.util :as util]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as str]))

(defn- run [input resonant?]
  (let [
      grid (str/split-lines input)
      antennae (reduce
        (fn [map [char x y]] (merge-with into map {char [[x y]]}))
        {}
        (apply concat
          (map-indexed
            (fn [y line]
              (remove
                (fn [[char]] (= char \.))
                (map-indexed
                  (fn [x char] [char x y])
                  line
                )
              )
            )
            grid
          )
        )
      )
    ]
    (util/count-matches
      (fn [[x y]]
        (some
          (fn [[_ positions]]
            (some
              (fn [[[x1 y1] [x2 y2]]]
                (resonant? (- x x1) (- y y1) (- x2 x) (- y2 y))
              )
              (combinatorics/combinations positions 2)
            )
          )
          antennae
        )
      )
      (apply concat
        (map-indexed
          (fn [y line] (map (fn [x] [x y]) (range (count line))))
          grid
        )
      )
    )
  )
)

(defn day8-1 [input]
  (run input
    (fn [dx1 dy1 dx2 dy2]
      (or
        (and
          (= dx1 (* dx2 2))
          (= dy1 (* dy2 2))
        )
        (and
          (= (* dx1 2) dx2)
          (= (* dy1 2) dy2)
        )
        (and
          (= dx1 (* dx2 -2))
          (= dy1 (* dy2 -2))
        )
        (and
          (= (* dx1 -2) dx2)
          (= (* dy1 -2) dy2)
        )
      )
    )
  )
)

(defn day8-2 [input]
  (run input
    (fn [dx1 dy1 dx2 dy2]
      (cond
        (zero? dx1) (or (zero? dx2) (zero? dy1))
        (zero? dx2) (zero? dy2)
        :else (= (/ dy1 dx1) (/ dy2 dx2))
      )
    )
  )
)
