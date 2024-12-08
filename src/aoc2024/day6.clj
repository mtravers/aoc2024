(ns aoc2024.day6
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def arr (au/adata (ju/file-lines "data/day6.txt")))

(defn turn-right
  [d]
  (case d
    [1 0] [0 1]
    [0 1] [-1 0]
    [-1 0] [0 -1]
    [0 -1] [1 0]))

(defn travel
  [arr [i j] [di dj :as d] visited]
  (let [ni (+ i di)
        nj (+ j dj)]
    (cond (not (au/in-bounds? arr ni nj)) visited
          (= \# (au/rget arr ni nj))
          (recur arr [i j] (turn-right d) visited)
          :else (recur arr [ni nj] d (conj visited [ni nj])))))

(defn p1
  []
  (let [start (au/afind arr \^)]
    (count (travel arr start [0 -1] #{start}))))

;;; Brute force. Slow but works

(defn travel2
  [arr [i j :as p] [di dj :as d] visited]
  (if (contains? visited (concat p d))
    (throw (ex-info "loop" {}))
    (let [ni (+ i di)
          nj (+ j dj)]
      (cond (not (au/in-bounds? arr ni nj)) visited
            (= \# (au/rget arr ni nj))
            (recur arr p (turn-right d) visited)
            :else (recur arr [ni nj] d (conj visited (concat p d)))))))
(defn p2
  []
  (let [start (au/afind arr \^)
        ovisited (disj (travel arr start [0 -1] #{start}) start)]
    (reduce +
            (map (fn [added]
                   (try (count (travel2 (au/rset arr (first added) (second added) \#) start [0 -1] #{}))
                        0
                        (catch Exception e
                          1))
                   )
                 ovisited))))
  
