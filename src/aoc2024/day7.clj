(ns aoc2024.day7
  (:require [aoc2024.util :as util]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn- parse-input [input]
  (map
    (fn [line] (let [[result expr] (str/split line #": " 2)]
      [(parse-long result) (map parse-long (str/split expr #" "))]
    ))
    (str/split-lines input)
  )
)

(defn- run [input num-operators]
  (->> (parse-input input)
    (filter (fn [[result expr]]
      (some
        (fn [combo] (->> expr
          (map (fn [x] [0 x]))
          (reduce
            (fn [[i x] [_ y]] [
              (inc i)
              (case
                (mod (quot combo (util/powi num-operators i)) num-operators)
                0 (+ x y)
                1 (* x y)
                2 (+ y (* x (long (math/pow 10 (inc (math/floor (math/log10 y)))))))
              )
            ])
          )
          (last)
          (= result)
        ))
        (range (util/powi num-operators (dec (count expr))))
      )
    ))
    (map first)
    (reduce +)
  )
)

(defn day7-1 [input]
  (run input 2)
)

(defn day7-2 [input]
  (run input 3)
)
