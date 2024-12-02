(ns aoc2024.util)

(defn count-matches [func seq]
  (reduce + (map #(if (func %) 1 0) seq))
)

(defn count-occurrences [haystack needle]
  (count-matches #(= % needle) haystack)
)

(defn read-input []
  (slurp "input.txt")
)

(defn transpose [xs]
  (apply map list xs)
)

(defn remove-at [vec pos]
  (into (subvec vec 0 pos) (subvec vec (inc pos)))
)
