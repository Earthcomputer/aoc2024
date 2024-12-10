(ns aoc2024.day10
  (:require [clojure.string :as str]))

(def ^:private directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn- run [input func]
  (let [grid (vec (str/split-lines input))]
    (reduce +
      (map-indexed
        (fn [y line]
          (reduce +
            (map
              (fn [x] (if (= (get-in grid [y x]) \0) (func grid x y) 0))
              (range (count line))
            )
          )
        )
        grid
      )
    )
  )
)

(defn- count-nines [grid x y visited]
  (cond
    (contains? visited [x y]) [0 visited]
    (= (get-in grid [y x]) \9) [1 (conj visited [x y])]
    :else (let [n (inc (int (get-in grid [y x]))) visited (conj visited [x y])]
      (reduce
        (fn [[count visited] [dx dy]]
          (let [x1 (+ x dx) y1 (+ y dy) c (get-in grid [y1 x1])]
            (if
              (or (nil? c) (not= (int c) n))
              [count visited]
              (let [[new-count visited] (count-nines grid x1 y1 visited)]
                [(+ count new-count) visited]
              )
            )
          )
        )
        [0 visited]
        directions
      )
    )
  )
)

(defn day10-1 [input]
  (run input (fn [grid x y] (first (count-nines grid x y #{}))))
)

(defn count-paths [grid x y]
  (let [count-paths' (promise)]
    (deliver count-paths'
      (memoize
        (fn [x y]
          (let [c (get-in grid [y x])]
            (if (= c \9)
              1
              (reduce +
                (map
                  (fn [[dx dy]]
                    (let [x1 (+ x dx) y1 (+ y dy) c1 (get-in grid [y1 x1])]
                      (if (or (nil? c1) (not= (int c1) (inc (int c))))
                        0
                        (@count-paths' x1 y1)
                      )
                    )
                  )
                  directions
                )
              )
            )
          )
        )
      )
    )
    (@count-paths' x y)
  )
)

(defn day10-2 [input]
  (run input count-paths)
)
