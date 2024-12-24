(ns aoc2024.day16
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            [aoc2024.clerk :as clrk]
            ))

(def array (au/adata (ju/file-lines "data/day16.txt")))

;;; Correct but fails on p1 with stack overflow, sigh
(defn shortest-path
  [arr from to dir visited]
  (if (= from to) 
    0                                 
    (let [neighbors                   ;of to
          (->> (au/neighbors to)
               (remove #(= \# (au/rget arr %)))
               (remove #(contains? visited %)))
          nvisited (conj visited to)
          ]
      #_ (prn :goop to dir neighbors)
      (u/min* (u/mapf
               #(let [ndir (au/p- % to)
                      path (shortest-path arr from % ndir nvisited)]
                  (when path
                    (+ path
                       1
                       (if (= ndir dir) 0 1000))))
               neighbors)))))

  

(defn p1
  [arr]
  (let [start (au/afind arr \S)
        end (au/afind arr \E)]
    (shortest-path arr start end nil #{})
    ))

(p1 array)

