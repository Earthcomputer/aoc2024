(ns aoc2024.day9)

(defn- expand-input [input]
  (vec
    (apply concat
      (map-indexed
        (fn [i count]
          (repeat (- (int count) (int \0)) (if (even? i) (/ i 2) nil))
        )
        input
      )
    )
  )
)

(defn- checksum [files]
  (reduce + (map-indexed (fn [i file] (if (nil? file) 0 (* i file))) files))
)

(defn day9-1 [input]
  (loop [
      expanded (expand-input input)
      dest-index 0
      src-index (dec (count expanded))
    ]
    (cond
      (<= src-index dest-index) (checksum expanded)
      (some? (get expanded dest-index)) (recur expanded (inc dest-index) src-index)
      (nil? (get expanded src-index)) (recur expanded dest-index (dec src-index))
      :else (recur (assoc expanded dest-index (get expanded src-index) src-index nil) (inc dest-index) (dec src-index))
    )
  )
)

(defn- count-range [coll min-index func]
  (count (take-while #(func (get coll %)) (iterate inc min-index)))
)

(defn day9-2 [input]
  (loop [
      expanded (expand-input input)
      src-index (dec (count expanded))
      min-dest-index 0
    ]
    (cond
      (<= src-index min-dest-index) (checksum expanded)
      (or (nil? (get expanded src-index)) (= (get expanded src-index) (get expanded (dec src-index)))) (recur expanded (dec src-index) min-dest-index)
      :else (let [
          file-id (get expanded src-index)
          len (count-range expanded src-index #(= % file-id))
          dest-index (loop [dest-index min-dest-index]
            (cond
              (>= dest-index src-index) nil
              (>= (count-range expanded dest-index nil?) len) dest-index
              :else (recur (inc dest-index))
            )
          )
        ]
        (if (nil? dest-index)
          (recur expanded (dec src-index) min-dest-index)
          (let [
              expanded (mapv
                (fn [i]
                  (cond
                    (<= src-index i (+ src-index (dec len))) nil
                    (<= dest-index i (+ dest-index (dec len))) file-id
                    :else (get expanded i)
                  )
                )
                (range (count expanded))
              )
            ]
            (recur expanded (dec src-index) (+ min-dest-index (count-range expanded min-dest-index some?)))
          )
        )
      )
    )
  )
)
