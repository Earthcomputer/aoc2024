(ns aoc2024.day2
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(defn- parse-input [input]
  (map (fn [line] (mapv parse-long (str/split line #" "))) (str/split-lines input))
)

(defn- is-safe [report]
  (let [windows (partition 2 1 report)]
    (or
      (every? (fn [[left right]] (<= 1 (- right left) 3)) windows)
      (every? (fn [[left right]] (<= -3 (- right left) -1)) windows)
    )
  )
)

(defn- is-safe-damper [report]
  (some
    (fn [i] (is-safe (util/remove-at report i)))
    (range (count report))
  )
)

(defn day2-1 [input]
  (->> input
    (parse-input)
    (util/count-matches is-safe)
  )
)

(defn day2-2 [input]
  (->> input
    (parse-input)
    (util/count-matches is-safe-damper)
  )
)
