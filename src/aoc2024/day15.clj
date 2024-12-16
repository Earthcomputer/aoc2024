(ns aoc2024.day15
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions {\< [-1 0] \^ [0 -1] \> [1 0] \v [0 1]})

(defn- parse-input [input]
  (let [
      lines (str/split-lines input)
      [grid instructions] (split-with #(not (empty? %)) lines)
      grid (vec grid)
      instructions (str/join instructions)
    ]
    [grid instructions]
  )
)

(defn- can-move1 [grid robot-x robot-y dx dy]
  (= \.
    (first
      (filter
        #(or (= \. %) (= \# %))
        (map (fn [[x y]] (get-in grid [y x])) (drop 1 (iterate (partial mapv + [dx dy]) [robot-x robot-y])))
      )
    )
  )
)

(defn- move1 [grid robot-x robot-y instruction]
  (let [[dx dy] (get directions instruction)]
    (if (can-move1 grid robot-x robot-y dx dy)
      (let [
          grid (reduce
            (fn [grid [x y]] (util/set-in-grid (util/set-in-grid grid [(+ y dy) (+ x dx)] \O) [y x] \.))
            grid
            (reverse
              (take-while
                (fn [[x y]] (= \O (get-in grid [y x])))
                (drop 1 (iterate (partial mapv + [dx dy]) [robot-x robot-y]))
              )
            )
          )
        ]
        [grid (+ robot-x dx) (+ robot-y dy)]
      )
      [grid robot-x robot-y]
    )
  )
)

(defn- can-move2 [grid robot-x robot-y dx dy]
  (let [
      x (+ robot-x dx)
      y (+ robot-y dy)
    ]
    (case (get-in grid [y x])
      \# false
      \[ (and (can-move2 grid x y dx dy) (or (zero? dy) (can-move2 grid (inc x) y dx dy)))
      \] (and (can-move2 grid x y dx dy) (or (zero? dy) (can-move2 grid (dec x) y dx dy)))
      true
    )
  )
)

(defn- horizontal-positions-to-move [grid robot-x robot-y dx]
  (->> (iterate (partial + dx) robot-x)
    (drop 1)
    (take-while #(or (= \[ (get-in grid [robot-y %])) (= \] (get-in grid [robot-y %]))))
    (map (fn [x] [x robot-y]))
  )
)

(defn- vertical-positions-to-move [grid robot-x robot-y dy positions]
  (let [
      x robot-x
      y (+ robot-y dy)
      c (get-in grid [y x])
    ]
    (cond
      (contains? positions [x y]) positions
      (= c \.) positions
      (= c \[) (vertical-positions-to-move grid x y dy (vertical-positions-to-move grid (inc x) y dy (conj positions [x y] [(inc x) y])))
      (= c \]) (vertical-positions-to-move grid x y dy (vertical-positions-to-move grid (dec x) y dy (conj positions [x y] [(dec x) y])))
    )
  )
)

(defn- move2 [grid robot-x robot-y dx dy]
  (let [
      positions (if (zero? dy) (horizontal-positions-to-move grid robot-x robot-y dx) (vertical-positions-to-move grid robot-x robot-y dy #{}))
      positions (sort-by #(- (reduce + (mapv * [dx dy] %))) positions)
    ]
    (reduce
      (fn [grid [x y]]
        (util/set-in-grid (util/set-in-grid grid [(+ y dy) (+ x dx)] (get-in grid [y x])) [y x] \.)
      )
      grid
      positions
    )
  )
)

(defn- try-move2 [grid robot-x robot-y instruction]
  (let [[dx dy] (get directions instruction)]
    (if (can-move2 grid robot-x robot-y dx dy)
      [(move2 grid robot-x robot-y dx dy) (+ robot-x dx) (+ robot-y dy)]
      [grid robot-x robot-y]
    )
  )
)

(defn- run [grid instructions move-func]
  (let [
      [robot-x robot-y] (util/find-in-grid grid #(= \@ %))
      grid (util/set-in-grid grid [robot-y robot-x] \.)
      [grid] (reduce
        (fn [[grid robot-x robot-y] instruction] (move-func grid robot-x robot-y instruction))
        [grid robot-x robot-y]
        instructions
      )
    ]
    (reduce +
      (map
        (fn [[x y]] (+ x (* y 100)))
        (filter
          (fn [[x y]] (or (= \O (get-in grid [y x])) (= \[ (get-in grid [y x]))))
          (util/grid-coords grid)
        )
      )
    )
  )
)

(defn day15-1 [input]
  (let [[grid instructions] (parse-input input)]
    (run grid instructions move1)
  )
)

(defn day15-2 [input]
  (let [
      mapping {\# "##" \O "[]" \. ".." \@ "@."}
      [grid instructions] (parse-input input)
      grid (mapv (fn [line] (str/join (map mapping line))) grid)
    ]
    (run grid instructions try-move2)
  )
)
