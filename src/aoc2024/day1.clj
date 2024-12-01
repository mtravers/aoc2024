(ns aoc2024.day1
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(defn split-tokens
  [s]
  (s/split s #" +"))

(defn data
  []
  (let [rawish (->> "data/day1.txt"
                    ju/file-lines
                    (map split-tokens))
        a (sort (map (comp u/coerce-numeric first) rawish))
        b (sort (map (comp u/coerce-numeric second) rawish))]
    [a b]))

(defn p1
  []
  (let [[a b] (data)]
    (reduce + (map #(abs (- %1 %2)) a b))))

(defn p2
  []
  (let [[a b] (data)
        bf (frequencies b)]
    (reduce + (map #(* %1 (get bf %1 0)) a))))

       
