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

(defn index-of-fn [vec pred]
  (->> vec
    (map-indexed (fn [i x] (if (pred x) i nil)))
    (filter some?)
    (first)
  )
)

(defn index-of [vec x]
  (index-of-fn vec #(= x %))
)

(defn powi [base exp]
  (loop [base base exp exp result 1]
    (cond
      (= exp 0) result
      (= exp 1) (* result base)
      (even? exp) (recur (* base base) (bit-shift-right exp 1) result)
      :else (recur (* base base) (bit-shift-right exp 1) (* result base))
    )
  )
)

(defn dbg [x]
  (println x)
  x
)
