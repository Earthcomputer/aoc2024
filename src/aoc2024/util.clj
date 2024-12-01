(ns aoc2024.util)

(defn count-occurrences [haystack needle]
  (reduce + (map (fn [x] (if (= x needle) 1 0)) haystack))
)

(defn read-input []
  (slurp "input.txt")
)

(defn transpose [xs]
  (apply map list xs)
)
