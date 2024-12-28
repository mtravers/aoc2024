(ns aoc2024.day19
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            [aoc2024.clerk :as clrk]
            ))

;;; Super-easy 

(defn prefix=
  [pre str]
  (and (>= (count str) (count pre))
       (= pre (subs str 0 (count pre)))))     ;Note: ineffeciaent

(u/defn-memoized possible?
  [vocab str]
  (or (empty? str)
      (some (fn [elt]
              (and (prefix= elt str)
                   (possible? vocab (subs str (count elt)) )))
            vocab)))

(def test-vocab  ["r", "wr", "b", "g", "bwu", "rb", "gb", "br"])

(def test-words
  [
   "brwrr"
   "bggr"
   "gbbr"
   "rrbgbr"
   "ubwu"
   "bwurrg"
   "brgr"
   "bbrgwb"])

(defn read-data
  []
  (let [lines (ju/file-lines "data/day19.txt")
        vocab (s/split (first lines)  #", ")
        patterns (drop 2 lines)]
    [vocab patterns]))

;;; Naive approach fails, but works when memoized
(defn p1
  []
  (let [[vocab patterns] (read-data)]
    (count (u/mapf #(do (prn %) (possible? vocab %)) patterns))))

(u/defn-memoized counts
  [vocab str]
  (if (empty? str)
    1
    (reduce + (map
               (fn [elt]
                 (if (prefix= elt str)
                   (counts vocab (subs str (count elt)) )
                   0))
               vocab))))

(defn p2
  []
  (let [[vocab patterns] (read-data)]
    (reduce +
            (map #(do (prn %) (counts vocab %)) patterns))))
