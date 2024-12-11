(ns aoc2024.day7
  (:require [aoc2024.util :as util]
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
                (mod (quot combo (util/ipow num-operators i)) num-operators)
                0 (+ x y)
                1 (* x y)
                2 (+ y (* x (util/ipow10 (util/count-digits y))))
              )
            ])
          )
          (last)
          (= result)
        ))
        (range (util/ipow num-operators (dec (count expr))))
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
