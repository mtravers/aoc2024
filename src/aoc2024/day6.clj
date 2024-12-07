(ns aoc2024.day6
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def arr (au/adata (ju/file-lines "data/day6x.txt")))

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
    (cond (not (au/in-bounds? arr ni nj)) (count visited)
          (= \# (au/rget arr ni nj))
          (recur arr [i j] (turn-right d) visited)
          :else (recur arr [ni nj] d (conj visited [ni nj])))))

(defn travel
  [arr [i j] [di dj :as d] visited]
  (let [ni (+ i di)
        nj (+ j dj)]
    (cond (not (au/in-bounds? arr ni nj)) (count visited)
          (= \# (au/rget arr ni nj))
          (recur arr [i j] (turn-right d) visited)
          :else (recur arr [ni nj] d (conj visited [ni nj])))))

(defn p1
  []
  (let [start (au/afind arr \^)]
    (travel arr start [0 -1] #{start})))

;;; Brute force
(defn travel2
  [arr [i j :as p] [di dj :as d] visited]
  (prn :t2 p)
  (if (contains? visited p)
    (throw (ex-info "loop" {}))
    (let [ni (+ i di)
          nj (+ j dj)]
      (cond (not (au/in-bounds? arr ni nj)) visited
            (= \# (au/rget arr ni nj))
            (recur arr p (turn-right d) visited)
            :else (recur arr [ni nj] d (conj visited [ni nj]))))))
(defn p2
  []
  (let [start (au/afind arr \^)
        visited (travel2 arr start [0 -1] #{start})]
    (map (fn [added]
           (try (count (travel2 (au/rset arr (first added) (second added) \#) start [0 -1] #{start}))
                (catch Exception e
                  :loop)))
         visited)))
  
