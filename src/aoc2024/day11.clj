(ns aoc2024.day11
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(defn- simulate [stones]
  (reduce
    (partial merge-with +)
    {}
    (map
      (fn [[n count]]
        (if (zero? n)
          {1 count}
          (let [num-digits (util/count-digits n)]
            (if (even? num-digits)
              (let [
                  pow10 (util/ipow10 (/ num-digits 2))
                  a (quot n pow10)
                  b (mod n pow10)
                ]
                (if (= a b)
                  {a (* count 2)}
                  {a count b count}
                )
              )
              {(* 2024 n) count}
            )
          )
        )
      )
      stones
    )
  )
)

(defn- run [input count]
  (let [
      stones (map parse-long (str/split input #" "))
      stones (reduce
        #(merge-with + %1 {%2 1})
        {}
        stones
      )
    ]
    (reduce + (map last (nth (iterate simulate stones) count)))
  )
)

(defn day11-1 [input]
  (run input 25)
)

(defn day11-2 [input]
  (run input 75)
)
