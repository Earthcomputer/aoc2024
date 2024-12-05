(ns aoc2024.day5
  (:require [clojure.string :as str]))

(defn- parse-input [input]
  (let [
      lines (str/split-lines input)
      [rules page-lists] (split-with #(not (empty? %)) lines)
      page-lists (drop 1 page-lists)
    ]
    [
      (map (fn [rule] (mapv parse-long (str/split rule #"\|"))) rules)
      (map (fn [page-list] (mapv parse-long (str/split page-list #","))) page-lists)
    ]
  )
)

(defn- wrong-order? [reverse-graph a b]
  (contains? (get reverse-graph a #{}) b)
)

(defn- valid? [reverse-graph page-list]
  (let [windows (partition 2 1 page-list)]
    (not-any? (fn [[a b]] (wrong-order? reverse-graph a b)) windows)
  )
)

(defn- run [input process-page-list-func]
  (let [
      [rules page-lists] (parse-input input)
      reverse-graph (reduce
        (fn [map [a b]] (merge-with into map {b #{a}}))
        {}
        rules
      )
    ]
    (reduce + (map #(process-page-list-func reverse-graph %) page-lists))
  )
)

(defn day5-1 [input]
  (run input
    (fn [reverse-graph page-list]
      (if
        (valid? reverse-graph page-list)
        (get page-list (quot (count page-list) 2))
        0
      )
    )
  )
)

(defn day5-2 [input]
  (run input
    (fn [reverse-graph page-list]
      (if
        (valid? reverse-graph page-list)
        0
        (let
          [
            page-list (sort
              (fn [a b]
                (cond
                  (wrong-order? reverse-graph a b) 1
                  (wrong-order? reverse-graph b a) -1
                  :else 0
                )
              )
              page-list
            )
            page-list (vec page-list)
          ]
          (get page-list (quot (count page-list) 2))
        )
      )
    )
  )
)
