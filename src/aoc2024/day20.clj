(ns aoc2024.day20
  (:require [aoc2024.util :as util]
            [clojure.string :as str]))

(def ^:private directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn- bfs [grid start-x start-y end-x end-y]
  (loop [
      queue [[start-x start-y]]
      distances {[start-x start-y] 0}
    ]
    (let [[[x y]] queue]
      (if (or (nil? x) (= [x y] [end-x end-y]))
        distances
        (let [
            [queue distances] (reduce
              (fn [[queue distances] [dx dy]]
                (let [x1 (+ x dx) y1 (+ y dy)]
                  (if
                    (or
                      (= \# (get-in grid [y1 x1]))
                      (and (contains? distances [x1 y1]) (>= (inc (get distances [x y])) (get distances [x1 y1])))
                    )
                    [queue distances]
                    [(conj queue [x1 y1]) (assoc distances [x1 y1] (inc (get distances [x y])))]
                  )
                )
              )
              [(vec (rest queue)) distances]
              directions
            )
          ]
          (recur queue distances)
        )
      )
    )
  )
)

(defn- run [input max-cheat-time]
  (let [
      grid (str/split-lines input)
      [start-x start-y] (util/find-in-grid grid #(= % \S))
      [end-x end-y] (util/find-in-grid grid #(= % \E))
      distances (bfs grid start-x start-y end-x end-y)
    ]
    (reduce +
      (map
        (fn [[x y]]
          (let [dist1 (get distances [x y])]
            (if (nil? dist1)
              0
              (reduce +
                (map
                  (fn [dy]
                    (util/count-matches
                      (fn [dx]
                        (and
                          (<= (+ (abs dx) (abs dy)) max-cheat-time)
                          (let [
                              dist2 (get distances [(+ x dx) (+ y dy)])
                            ]
                            (and (some? dist1) (some? dist2) (>= dist2 (+ dist1 100 (abs dx) (abs dy))))
                          )
                        )
                      )
                      (range (- max-cheat-time) (inc max-cheat-time))
                    )
                  )
                  (range (- max-cheat-time) (inc max-cheat-time))
                )
              )
            )
          )
        )
        (util/grid-coords grid)
      )
    )
  )
)

(defn day20-1 [input]
  (run input 2)
)

(defn day20-2 [input]
  (run input 20)
)
