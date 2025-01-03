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

(defn amap
  [f a]
  (mapv #(mapv f %) a))

(def p+ (u/vectorize +))
(def p- (u/vectorize -))

(defn chari
  [c]
  (- (int c) (int \0)))

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
  ([data x y]
   (get-in data [y x]))
  ([data [x y]]
   (get-in data [y x])))

(defn rset
  ([data x y v]
   (assoc-in data [y x] v))
  ([data [x y] v]
   (assoc-in data [y x] v)))

(defn rset*
  [data points v]
  (reduce (fn [arr p] (rset arr p v)) data points))

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

(defn afind-all
  [arr c]
  (filter #(= c (rget arr %)) (all-points arr)))


(defn delete-pos
  [seq i]
  (into (subvec seq 0 i) (subvec seq (inc i))))

(defn delete-elt
  [seq elt]
  (u/remove= elt seq))


(defn blank-array
  [xs ys]
  (vec (repeat ys (vec (repeat xs \.)))))

(defn print-array
  [data]
  (doseq [line data]
    (println (apply str line))))

(defn neighbors
  [[i j]]
  [[(inc i) j] 
   [(dec i) j]
   [i (inc j)]
   [i (dec j)]])

(def neighbors-v
  [[1  0] 
   [-1 0]
   [0 1]
   [0 -1]])

(defn str-binary
  [n]
  (Long/toString n 2))

(def zeros (apply str (repeat 100 \0)))

(defn str-binary-pad
  [n s]
  (let [b (str-binary n)]
    (str (subs zeros 0 (- s (count b))) b)))

(defn read-binary
  [s]
  (read-string (str "2r" s)))

(defn print-binary
  [n]
  (print (Integer/toString n 2)))
