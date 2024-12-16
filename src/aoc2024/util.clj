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

(defn grid-coords [grid]
  (apply concat (map-indexed (fn [y line] (map (fn [x] [x y]) (range (count line)))) grid))
)

(defn find-in-grid [grid predicate]
  (first (filter (fn [[x y]] (predicate (get-in grid [y x]))) (grid-coords grid)))
)

(defn shortest-distances-from
  "Computes the shortest distances from 'from' to the first node that matches 'stop-at', and all nodes closer than that
  node. Returns a pair containing the node stopped at and a map from node to distance. 'adjacent-func' takes a node as
  input and returns an iterable of pairs whose first values are the adjacent nodes and second values are the distances
  to the adjacent nodes."
  ([from adjacent-func] (shortest-distances-from from #(false) adjacent-func))
  ([from stop-at adjacent-func]
    (loop [
        distances {from 0}
        to-visit (sorted-set [0 from])
      ]
      (let [[dist node] (first to-visit)]
        (cond
          (nil? dist) [nil distances]
          (stop-at node) [node distances]
          :else (let [
              adjacent (map (fn [[n d]] [n (+ dist d)]) (adjacent-func node))
              to-visit (into
                (disj to-visit [dist node])
                (map
                  (fn [[n d]] [d n])
                  (filter
                    (fn [[n d]]
                      (let [prev-d (get distances n)]
                        (or (nil? prev-d) (< d prev-d))
                      )
                    )
                    adjacent
                  )
                )
              )
              distances (merge-with min distances (into {} adjacent))
            ]
            (recur distances to-visit)
          )
        )
      )
    )
  )
)

(defn shortest-distance
  "Computes the shortest distance from 'from' to any node that matches 'is-dest?'. 'adjacent-func' takes a node as input
  and returns an iterable of pairs whose first values are the adjacent nodes and second values are the distances to the
  adjacent nodes."
  [from is-dest? adjacent-func]
  (let [[dest distances] (shortest-distances-from from is-dest? adjacent-func)] (get distances dest))
)

(defn dbg [x]
  (println x)
  x
)
