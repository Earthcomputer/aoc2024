(ns aoc2024.day23
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as str]))

(defn- parse-input [input]
  (->> (str/split-lines input)
    (map (fn [line]
      (let [[a b] (str/split line #"-" 2)]
        [{a #{b}} {b #{a}}]
      )
    ))
    (apply concat)
    (apply merge-with into)
  )
)

(defn day23-1 [input]
  (let [
      graph (parse-input input)
      triples (set
        (apply concat
          (map
            (fn [[a values]]
              (map
                (fn [[b c]] #{a b c})
                (filter
                  (fn [[b c]] (contains? (get graph b) c))
                  (combinatorics/combinations values 2)
                )
              )
            )
            graph
          )
        )
      )
    ]
    (count (filter (fn [triple] (some #(str/starts-with? % "t") triple)) triples))
  )
)

(defn- is-complete [graph subgraph]
  (every? (fn [[a b]] (contains? (get graph a) b)) (combinatorics/combinations subgraph 2))
)

(defn day23-2 [input]
  (let [
      graph (parse-input input)
      max-subgraph-size (inc (apply max (map count (vals graph))))
    ]
    (->> (range max-subgraph-size 0 -1)
      (map
        (fn [subgraph-size]
          (filter
            #(not (empty? %))
            (map
              (fn [[key val]]
                (filter
                  (fn [subgraph]
                    (and
                      (= key (reduce (fn [a b] (if (neg? (compare a b)) a b)) subgraph))
                      (is-complete graph subgraph)
                    )
                  )
                  (combinatorics/combinations (vec (conj val key)) subgraph-size)
                )
              )
              graph
            )
          )
        )
      )
      (filter #(not (empty? %)))
      (first)
      (first)
      (first)
      (sort)
      (str/join ",")
    )
  )
)
