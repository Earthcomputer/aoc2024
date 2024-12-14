(ns aoc2024.day14
  (:require [clojure.string :as str]))

(def ^:private WIDTH 101)
(def ^:private HEIGHT 103)

(defn- parse-input [input]
  (map
    (fn [line]
      (vec (map parse-long (drop 1 (re-matches #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" line))))
    )
    (str/split-lines input)
  )
)

(defn- quadrant-counts [iterations input]
  (->> input
    (map
      (fn [[x y vx vy]]
        (let [x (mod (+ x (* iterations vx)) WIDTH) y (mod (+ y (* iterations vy)) HEIGHT)]
          (cond
            (< x (quot WIDTH 2)) (cond
              (< y (quot HEIGHT 2)) [1 0 0 0]
              (> y (quot HEIGHT 2)) [0 1 0 0]
              :else [0 0 0 0]
            )
            (> x (quot WIDTH 2)) (cond
              (< y (quot HEIGHT 2)) [0 0 1 0]
              (> y (quot HEIGHT 2)) [0 0 0 1]
              :else [0 0 0 0]
            )
            :else [0 0 0 0]
          )
        )
      )
    )
    (reduce
      (partial mapv +)
      [0 0 0 0]
    )
  )
)

(defn- run [input iterations]
  (->> input
    (quadrant-counts iterations)
    (reduce *)
  )
)

(defn- is-christmas-tree [iterations input]
  ; checks if there are 15 robots in a line
  ; terrible heuristic for a terrible puzzle
  ; had to look at other people's answers to understand what this meant
  (let [positions (into #{} (map (fn [[x y vx vy]] [(mod (+ x (* iterations vx)) WIDTH) (mod (+ y (* iterations vy)) HEIGHT)]) input))]
    (some (fn [[x y]] (every? #(contains? positions [(+ x %) y]) (range 15))) positions)
  )
)

(defn day14-1 [input]
  (run (parse-input input) 100)
)

(defn day14-2 [input]
  (let [input (parse-input input)]
    (first (filter #(is-christmas-tree % input) (iterate inc 0)))
  )
)
