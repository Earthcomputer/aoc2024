(ns aoc2024.day24
  (:require [clojure.string :as str]))

(defn- parse-input [input]
  (let [
      lines (str/split-lines input)

      [initial-states gates] (split-with #(not (empty? %)) lines)
      gates (drop 1 gates)

      initial-states (into {}
        (map
          (fn [line]
            (let [[key val] (str/split line #": " 2)]
              [key (= val "1")]
            )
          )
          initial-states
        )
      )
      gates (mapv #(vec (drop 1 (re-matches #"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" %))) gates)
    ]
    [initial-states gates]
  )
)

(defn- read-output [letter state]
  (->> state
    (filter (fn [[k]] (str/starts-with? k letter)))
    (map (fn [[k v]] (if v (bit-shift-left 1 (parse-long (subs k 1))) 0)))
    (reduce bit-or)
  )
)

(defn- simulate [state gates]
  (loop [state state]
    (let [
        [changed state] (reduce
          (fn [[changed state] [in1 op in2 out]]
            (if (or (contains? state out) (not (contains? state in1)) (not (contains? state in2)))
              [changed state]
              (let [
                  val (case op
                    "AND" (and (get state in1) (get state in2))
                    "OR" (or (get state in1) (get state in2))
                    "XOR" (not= (get state in1) (get state in2))
                  )
                ]
                [true (assoc state out val)]
              )
            )
          )
          [false state]
          gates
        )
      ]
      (if changed
        (recur state)
        (read-output "z" state)
      )
    )
  )
)

(defn day24-1 [input]
  (let [[state gates] (parse-input input)]
    (simulate state gates)
  )
)

(defn day24-2 [input]
  (let [
      [state gates] (parse-input input)
      x (read-output "x" state)
      y (read-output "y" state)
      z (simulate state gates)
      expected_z (+ x y)
    ]
    (println "// Expected :" (Long/toBinaryString expected_z))
    (println "// Actual   :" (Long/toBinaryString z))
    (println "// Good luck!")
    (println)
    (println "digraph aoc2024_day24 {")
    (doall
      (map
        (fn [[in1 op in2 out]]
          (println (str "GATE_" out " [label=\"" op "\"];"))
          (println (str in1 " -> GATE_" out ";"))
          (println (str in2 " -> GATE_" out ";"))
          (println (str "GATE_" out " -> " out ";"))
        )
        gates
      )
    )
    (println "}")
  )
)
