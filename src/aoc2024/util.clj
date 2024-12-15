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

(defn ipow [base exp]
  {:pre [(not (neg? exp))]}
  (loop [base base exp exp result 1]
    (cond
      (= exp 0) result
      (= exp 1) (* result base)
      (even? exp) (recur (* base base) (bit-shift-right exp 1) result)
      :else (recur (* base base) (bit-shift-right exp 1) (* result base))
    )
  )
)

(def ^:private DIGITS_LOG10 [
   0  0  0  1  1  1  2  2  2  3
   3  3  3  4  4  4  5  5  5  6
   6  6  6  7  7  7  8  8  8  9
   9  9  9 10 10 10 11 11 11 12
  12 12 12 13 13 13 14 14 14 15
  15 15 15 16 16 16 17 17 17 18
  18 18 18
])

(def ^:private POWERS_OF_10 (vec (take 19 (iterate #(* % 10) 1))))

(defn ipow10 [n]
  {:pre [(<= 0 n 18)]}
  (get POWERS_OF_10 n)
)

; https://stackoverflow.com/q/55032982/11071180
(defn ilog10 [n]
  {:pre [(<= 1 n Long/MAX_VALUE)]}
  (let [
      lz (bit-xor 63 (Long/numberOfLeadingZeros n))
      guess (get DIGITS_LOG10 lz)
    ]
    (if (< n (get POWERS_OF_10 guess)) (dec guess) guess)
  )
)

(defn count-digits [n]
  (if (zero? n) 1 (inc (ilog10 n)))
)

(defn set-in-grid [grid pos value]
  (let [[y x] pos]
    (vec (map-indexed (fn [yi line] (if (= yi y) (str (subs line 0 x) value (subs line (inc x))) line)) grid))
  )
)

(defn dbg [x]
  (println x)
  x
)
