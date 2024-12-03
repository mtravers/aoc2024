(ns aoc2024.day2
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au])
  )

(defn data
  []
  (->> "data/day2.txt"
       ju/file-lines
       (map au/split-tokens-numeric)
       (map vec)))

(defn sign
  [n]
  (cond (zero? n) 0
        (pos? n) 1
        :else -1))

(defn diffs
  [seq]
  (map - seq (rest seq)))

(defn desc-or-asc?
  [seq]
  (= 1 (count (distinct (map sign (diffs seq))))))

(defn deltas-in-range?
  [seq]
  (every? #(<= 1 % 3) (map abs (diffs seq))))

(defn safe?
  [seq]
  (and (desc-or-asc? seq)
       (deltas-in-range? seq)))

(defn p1
  []
  (count (u/mapf safe? (data))))

(defn delete-pos
  [seq i]
  (into (subvec seq 0 i) (subvec seq (inc i))))

(defn almost-safe?
  [seq]
  (or (safe? seq)
      (some (fn [i]
               (safe? (delete-pos seq i)))
             (range (count seq)))))

(defn p2
  []
  (count (u/mapf almost-safe? (data))))
