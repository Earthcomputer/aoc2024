(ns aoc2024.day3)

(defn day3-1 [input]
  (->> input
    (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
    (map (fn [[_ left right]] (* (parse-long left) (parse-long right))))
    (reduce +)
  )
)

(defn day3-2 [input]
  (let [matcher (re-matcher #"(don't)|(do)|mul\((\d{1,3}),(\d{1,3})\)" input)]
    (loop
      [
        [_ don't do left right] (re-find matcher)
        enabled true
        result 0
      ]
      (cond
        don't (recur (re-find matcher) false result)
        do (recur (re-find matcher) true result)
        left (recur
          (re-find matcher)
          enabled
          (if enabled (+ result (* (parse-long left) (parse-long right))) result)
        )
        :else result
      )
    )
  )
)
