(ns aoc2024.day14
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def data (->> "data/day14.txt"
                ju/file-lines
                (map #(re-seq #"[\d-]+" %))
                (map (u/vectorize u/coerce-numeric))))

(defn final-pos
  [x y dx dy]
  [(mod (+ x (* dx 100)) 101)
   (mod (+ y (* dy 100)) 103)])

(defn quadrant
  [[x y]]
  (cond (and (< x 50) (< y 51)) 1
        (and (> x 50) (< y 51)) 2
        (and (> x 50) (> y 51)) 3
        (and (< x 50) (> y 51)) 4
        :else nil))

(defn p1
  []
  (let [f (->> data
               (map #(apply final-pos %))
               (map quadrant)
               frequencies)]
    (reduce * (vals (dissoc f nil)))))


(defn blank-array
  [xs ys]
  (vec (repeat ys (vec (repeat xs \.)))))

(defn array-from-points
  [points]
  (loop [arry (blank-array 101 103)
         [p & rest] points]
    (if p
      (recur (au/rset arry (first p) (second p) \*)
             rest)
      arry)))

(defn print-array
  [data]
  (doseq [line data]
    (println (apply str line))))


(defn array-step
  [[x y dx dy]]
  [(mod (+ x (* dx 1)) 101)
   (mod (+ y (* dy 1)) 103)
   dx
   dy])

(defn biggest-connected-row
  [points]
  (as-> points points
    (group-by second points)
    (map #(sort (distinct (map first %))) (vals points))
    (map #(u/partition-diff (fn [a b] (= b (inc a))) %) points) ;contiguous sequences
    (map #(u/max* (map count %)) points)
    (u/max* points)
    ))

(defn p2
  []
  (loop [points data
         i 0]
    (if (> (biggest-connected-row points) 6)
      (do (print i) (print-array (array-from-points points)))
      (recur (map array-step points) (inc i)))))

