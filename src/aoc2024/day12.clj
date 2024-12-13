(ns aoc2024.day12
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def data (au/adata (ju/file-lines "data/day12.txt")))

(defn neighbors
  [[i j]]
  [[(inc i) j] 
   [(dec i) j]
   [i (inc j)]
   [i (dec j)]])

(defn find-region
  [data from char group assigned]
  (let [ch (au/rget data from)]
    (cond (not (au/in-bounds? data from)) group
          (not (= ch char)) group
          (contains? assigned from) group
          (contains? group from) group
          :else
          (reduce (fn [acc neighbor]
                    (find-region data neighbor char acc assigned))
                  (conj group from)
                  (neighbors from)))))

(defn find-regions
  [data]
  (loop [[p & rest-p] (au/all-points data)
         groups []
         visited #{}
         ]
    (cond (nil? p) groups
          (visited p) (recur rest-p groups visited)
          :else
          (let [g (find-region data p (au/rget data p) #{} visited)]
            (recur rest-p (conj groups g) (clojure.set/union g visited))))))

(defn region-perimeter
  [data region]
  (reduce
   +
   (map (fn [pt] (count (remove region (neighbors pt))))
        region)))
  
(defn region-score
  [data region]
  (* (region-perimeter data region)
     (count region)))

(defn p1
  []
  (reduce + (map (partial region-score data) (find-regions data))))

;;; P2

(defn count-segments
  [seq]
  (count (u/partition-diff (fn [a b] (= b (inc a))) seq)))

(defn region-segment-perimeter
  [data region]
  (reduce
   +
   (map (fn [[vec access blah]]
          (let [borders (remove region (map #(au/p+ % vec) region))
                groups (group-by blah borders)]
            (reduce +
                    (map (fn [borders] (count-segments (sort (map access borders)))) (vals groups)))))
        [[[0 1] first second]
         [[0 -1] first second]
         [[1 0] second first]
         [[-1 0] second first]])))

(defn region-score2
  [data region]
  (* (region-segment-perimeter data region)
     (count region)))

(defn p2
  []
  (reduce + (map (partial region-score2 data) (find-regions data))))
