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

(defn some-indexed
  [f coll & [i]]
  (if (empty? coll)
    nil
    (or (f (first coll) (or i 0))
        (some-indexed f (next coll) (inc (or i 0))))))

(defn afind
  [arr c]
  (some-indexed
   (fn [row i]
     (when-let [j (some-indexed (fn [ch j]
                           (when (= ch c)
                             j))
                         row)]
       [j i]))
   arr))

(defn rget
  [data x y]
  (get-in data [y x]))

(defn rset
  [data x y v]
  (assoc-in data [y x] v))

(defn split-nums
  [re s]
  (mapv u/coerce-numeric (s/split s re)))


(defn delete-pos
  [seq i]
  (into (subvec seq 0 i) (subvec seq (inc i))))

(defn delete-elt
  [seq elt]
  (u/remove= elt seq))
