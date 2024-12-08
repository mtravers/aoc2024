(ns aoc2024.utils
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

;;; [○][◍][ Sequences ][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍]

(defn some-indexed
  [f coll & [i]]
  (if (empty? coll)
    nil
    (or (f (first coll) (or i 0))
        (some-indexed f (next coll) (inc (or i 0))))))

(defn middle-elt
  [s]
  (nth s (/ (count s) 2)))

;;; this must in clj already?
(defn tails
  [s]
  (take-while seq (iterate rest s)))


;;; [○][◍][ Numeric ][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍]

(defn sign
  [n]
  (cond (zero? n) 0
        (pos? n) 1
        :else -1))

;;; [○][◍][ Strings ][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍]

(defn split-tokens
  [s]
  (s/split s #" +"))

(defn split-nums
  [s & [re]]
  (mapv u/coerce-numeric (s/split s (or re #"\D+"))))

(defn ncat
  "Lexical concate two numbers"
  [a b]
  (u/coerce-numeric (str a b)))

;;; [○][◍][ char arrays ][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍][○][◍]

(defn adata
  [data]
  (mapv vec data))

(defn adims
  [data]
  [(count (first data)) (count data)])

(defn all-points
  [data]
  (for [i (range (first (adims data)))
        j (range (second (adims data)))]
    [i j]))

(defn in-bounds?
  ([data x y]
   (let [[xs ys] (adims data)]
     (and (< -1 x xs) (< -1 y ys))))
  ([data p]
   (in-bounds? data (first p) (second p))))

(defn rget
  [data x y]
  (get-in data [y x]))

(defn rset
  [data x y v]
  (assoc-in data [y x] v))

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











(defn delete-pos
  [seq i]
  (into (subvec seq 0 i) (subvec seq (inc i))))

(defn delete-elt
  [seq elt]
  (u/remove= elt seq))



