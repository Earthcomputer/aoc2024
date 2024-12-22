(ns aoc2024.day21
  (:require [clojure.string :as str]))

(def ^:private door-layout {
  \7 [0 0]
  \8 [1 0]
  \9 [2 0]
  \4 [0 1]
  \5 [1 1]
  \6 [2 1]
  \1 [0 2]
  \2 [1 2]
  \3 [2 2]
  \0 [1 3]
  \A [2 3]
})
(def ^:private door-vals (set (vals door-layout)))

(def ^:private directions-layout {
  \^ [1 0]
  \A [2 0]
  \< [0 1]
  \v [1 1]
  \> [2 1]
})
(def ^:private directions-vals (set (vals directions-layout)))

(def ^:private get-shortest (memoize (fn [input keyboard keyboard-vals remaining-layers]
  (let [parts (str/split input #"A")]
    (if (<= (count parts) 1)
      (let [
          tries (bit-shift-left
            1
            (count
              (set
                (filter
                  (fn [[char1 char2]]
                    (and
                      (not= (first (get keyboard char1)) (first (get keyboard char2)))
                      (not= (last (get keyboard char1)) (last (get keyboard char2)))
                    )
                  )
                  (partition 2 1 (str \A input))
                )
              )
            )
          )
        ]
        (reduce min
          (map
            (fn [[next-layer]]
              (if (zero? remaining-layers)
                (count next-layer)
                (get-shortest next-layer directions-layout directions-vals (dec remaining-layers))
              )
            )
            (filter
              last
              (map
                (fn [try]
                  (let [
                      [next-layer _ valid] (reduce
                        (fn [[next-layer down-first-map valid try] [char1 char2]]
                          (let [
                              [x1 y1] (get keyboard char1)
                              [x2 y2] (get keyboard char2)
                              [down-first down-first-map try] (cond
                                (or (= x1 x2) (= y1 y2)) [false down-first-map try]
                                (contains? down-first-map [char1 char2]) [(get down-first-map [char1 char2]) down-first-map try]
                                :else [(pos? (bit-and try 1)) (assoc down-first-map [char1 char2] (zero? (bit-and try 1))) (bit-shift-right try 1)]
                              )
                              [next-layer valid] (if down-first
                                (if (or (contains? keyboard-vals [x1 y2]) (zero? remaining-layers))
                                  [
                                    (str
                                      next-layer
                                      (apply str (repeat (abs (- y2 y1)) (if (> y2 y1) \v \^)))
                                      (apply str (repeat (abs (- x2 x1)) (if (> x2 x1) \> \<)))
                                      \A
                                    )
                                    valid
                                  ]
                                  [next-layer false]
                                )
                                (if (or (contains? keyboard-vals [x2 y1]) (zero? remaining-layers))
                                  [
                                    (str
                                      next-layer
                                      (apply str (repeat (abs (- x2 x1)) (if (> x2 x1) \> \<)))
                                      (apply str (repeat (abs (- y2 y1)) (if (> y2 y1) \v \^)))
                                      \A
                                    )
                                    valid
                                  ]
                                  [next-layer false]
                                )
                              )
                            ]
                            [next-layer down-first-map valid try]
                          )
                        )
                        ["" {} true try]
                        (partition 2 1 (str \A input))
                      )
                    ]
                    [next-layer valid]
                  )
                )
                (range tries)
              )
            )
          )
        )
      )
      (reduce + (map #(get-shortest (str % \A) keyboard keyboard-vals remaining-layers) parts))
    )
  )
)))

(defn- run [input depth]
  (reduce +
    (map
      (fn [line]
        (*
          (get-shortest line door-layout door-vals depth)
          (parse-long (subs line 0 (dec (count line))))
        )
      )
      (str/split-lines input)
    )
  )
)

(defn day21-1 [input]
  (run input 2)
)

(defn day21-2 [input]
  (run input 25)
)
