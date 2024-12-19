(ns aoc2024.day19
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(defn- parse-input [input]
  (let [
      lines (str/split-lines input)
      patterns (str/split (first lines) #", ")
      designs (drop 2 lines)
    ]
    [patterns designs]
  )
)

(defn- is-design-possible [design patterns]
  (or
    (empty? design)
    (some
      (fn [pattern] (and
        (str/starts-with? design pattern)
        (is-design-possible (subs design (count pattern)) patterns)
      ))
      patterns
    )
  )
)

(defn day19-1 [input]
  (let [[patterns designs] (parse-input input)]
    (util/count-matches (fn [design] (is-design-possible design patterns)) designs)
  )
)

(defn- count-combinations [designs patterns]
  (let [count-combinations' (promise)]
    (deliver count-combinations' (memoize (fn [design]
      (if (empty? design)
        1
        (reduce +
          (map
            #(@count-combinations' (subs design (count %)))
            (filter #(str/starts-with? design %) patterns)
          )
        )
      )
    )))
    (reduce + (map @count-combinations' designs))
  )
)

(defn day19-2 [input]
  (let [[patterns designs] (parse-input input)]
    (count-combinations designs patterns)
  )
)
