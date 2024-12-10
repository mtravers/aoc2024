(ns aoc2024.day10
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

;;; Memoization isn't even needed

(def data (au/amap au/chari (au/adata (ju/file-lines "data/day10.txt"))))

(u/defn-memoized trailends
  [[i j :as p] expected]
  (let [actual (au/rget data i j)]
    (cond (not (= actual expected)) nil
          (= expected 9) #{p}
          :else
          (let [next (inc expected)]
            (clojure.set/union (trailends [(inc i) j] next)
                               (trailends [(dec i) j] next)
                               (trailends [i (inc j)] next)
                               (trailends [i (dec j)] next))))))

(defn p1
  []
  (reduce + (map #(count (trailends % 0)) (au/all-points data))))

(defn trailend-count
  [[i j :as p] expected]
  (let [actual (au/rget data i j)]
    (cond (not (= actual expected)) 0
          (= expected 9) 1
          :else
          (let [next (inc expected)]
            (+ (trailend-count [(inc i) j] next)
               (trailend-count [(dec i) j] next)
               (trailend-count [i (inc j)] next)
               (trailend-count [i (dec j)] next))))))

(defn p2
  []
  (reduce + (map #(trailend-count % 0) (au/all-points data))))
