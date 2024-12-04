(ns aoc2024.day4
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            ))

(defn adata
  [data]
  (mapv vec data))

(defn adims
  [data]
  [(count (first data)) (count data)])

(def arr (adata (ju/file-lines "data/day4.txt")))

(def arr (adata (ju/file-lines "data/day4-x.txt")))

(defn in-bounds
  [data x y]
  (let [[xs ys] (adims data)]
    (and (< -1 x xs) (< -1 y ys))))

(defn rget
  [data x y]
  (get-in data [y x]))

(defn is-string?
  [s x y dx dy]
  (cond (empty? s) true
        (not (in-bounds arr x y)) false
        :else (and (= (first s) (rget arr x y))
                   (is-string? (rest s) (+ x dx) (+ y dy) dx dy))))

(defn find-string-1
  [string dx dy]
  (let [s (into [] string)
        counter (atom 0)
        dims (adims arr)]
    (doseq [x (range (first dims))
            y (range (second dims))]
      (when (is-string? s x y dx dy)
        (swap! counter inc)))
    @counter))

(defn find-string
  [s]
  (+ (find-string-1 s 0 1)
     (find-string-1 s 0 -1)
     (find-string-1 s 1 1)
     (find-string-1 s 1 0)
     (find-string-1 s 1 -1)
     (find-string-1 s -1 1)
     (find-string-1 s -1 0)
     (find-string-1 s -1 -1)))
     
    
    

;;; Part 2

(def diags
  {[1 1] [[0 2 1 -1] [2 0 -1 1]]
   [-1 -1] [[0 -2 -1 1] [-2 0 1 -1]]
   [1 0] [[1 -1 0 1] [1 1 0 -1]]
   [-1 0] [[-1 -1 0 1] [-1 1 0 -1]]
   ;; [0 1] [[-1 1 1 0] [1 1  -1 0]]
   })

(defn find-string-1-p2
  [string dx dy]
  (let [s (into [] string)
        n (count s)
        dims (adims arr)
        counter (atom 0)]
    (doseq [x (range (first dims))
            y (range (second dims))]
      (when (is-string? s x y dx dy)
        (let [[[nx1 ny1 ndx1 ndy1] [nx2 ny2 ndx2 ndy2]] (get diags [dx dy])]
          (when (or (is-string? s (+ x nx1) (+ y ny1) ndx1 ndy1)
                    (is-string? s (+ x nx2) (+ y ny2) ndx2 ndy2))
            (swap! counter inc)))))
    @counter))


(defn find-string-p2
  [s]
  (+ (find-string-1-p2 s 1 1)
     (find-string-1-p2 s -1 -1)
     ;; Turns out you didn't need these
     #_ (find-string-1-p2 s 1 0)
     #_ (find-string-1-p2 s -1 0)
     ))
