(ns aoc2024.day16
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn- neighbors
  ([grid node] (neighbors grid node false))
  ([grid node reverse]
    (let [[x y dir] node]
      (into
        (let [[x1 y1] (mapv + [x y] (get directions (if reverse (bit-xor dir 2) dir)))]
          (if (= (get-in grid [y1 x1]) \#) [] [[[x1 y1 dir] 1]])
        )
        (map
          (fn [dir] [[x y dir] 1000])
          (remove #(= dir %) (range 4))
        )
      )
    )
  )
)

(defn day16-1 [input]
  (let [
      grid (str/split-lines input)
      [start-x start-y] (util/find-in-grid grid #(= \S %))
      end-pos (util/find-in-grid grid #(= \E %))
    ]
    (util/shortest-distance
      [start-x start-y 0]
      (fn [[x y]] (= [x y] end-pos))
      (fn [node] (neighbors grid node))
    )
  )
)

(defn- nodes-on-shortest-paths [grid distances from nodes]
  (let [distance (get distances from)]
    (reduce
      (fn [nodes [node dist]]
        (cond
          (contains? nodes node) nodes
          (= (- distance dist) (get distances node)) (nodes-on-shortest-paths grid distances node nodes)
          :else nodes
        )
      )
      (conj nodes from)
      (neighbors grid from true)
    )
  )
)

(defn day16-2 [input]
  (let [
      grid (str/split-lines input)
      [start-x start-y] (util/find-in-grid grid #(= \S %))
      end-pos (util/find-in-grid grid #(= \E %))
      [end-node distances] (util/shortest-distances-from
        [start-x start-y 0]
        (fn [[x y]] (= [x y] end-pos))
        (fn [node] (neighbors grid node))
      )
    ]
    (count (distinct (map (fn [[x y]] [x y]) (nodes-on-shortest-paths grid distances end-node #{}))))
  )
)
