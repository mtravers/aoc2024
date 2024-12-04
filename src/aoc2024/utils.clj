(ns aoc2024.utils
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(defn split-tokens
  [s]
  (s/split s #" +"))

(defn split-tokens-numeric
  [s]
  (map u/coerce-numeric (split-tokens s)))

(defn adata
  [data]
  (mapv vec data))

(defn adims
  [data]
  [(count (first data)) (count data)])

(defn in-bounds?
  [data x y]
  (let [[xs ys] (adims data)]
    (and (< -1 x xs) (< -1 y ys))))

(defn rget
  [data x y]
  (get-in data [y x]))


