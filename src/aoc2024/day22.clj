(ns aoc2024.day22
  (:require [clojure.string :as str]))

(defn- simulate-xorshift [value]
  (let [
      value (bit-and (bit-xor value (bit-shift-left value 6)) 0xffffff)
      value (bit-and (bit-xor value (bit-shift-right value 5)) 0xffffff)
      value (bit-and (bit-xor value (bit-shift-left value 11)) 0xffffff)
    ]
    value
  )
)

(defn day22-1 [input]
  (let [seeds (map parse-long (str/split-lines input))]
    (reduce + (map #(nth (iterate simulate-xorshift %) 2000) seeds))
  )
)

(defn- all-patterns [seeds]
  (set
    (apply concat
      (map
        (fn [seed]
          (map
            (fn [window] (mapv (fn [[a b]] (- b a)) (partition 2 1 window)))
            (partition 5 1 (take 2001 (map #(rem % 10) (iterate simulate-xorshift seed))))
          )
        )
        seeds
      )
    )
  )
)

(defn day22-2 [input]
  (let [
      seeds (map parse-long (str/split-lines input))
      pattern-prices (map
        (fn [seed]
          (reduce
            (fn [m [k v]] (if (contains? m k) m (assoc m k v)))
            {}
            (map
              (fn [window]
                [(mapv (fn [[a b]] (- b a)) (partition 2 1 window)) (last window)]
              )
              (partition 5 1 (map #(rem % 10) (take 2001 (iterate simulate-xorshift seed))))
            )
          )
        )
        seeds
      )
    ]
    (reduce max
      (map
        (fn [pattern]
          (reduce +
            (map
              (fn [pattern-prices] (get pattern-prices pattern 0))
              pattern-prices
            )
          )
        )
        (all-patterns seeds)
      )
    )
  )
)
