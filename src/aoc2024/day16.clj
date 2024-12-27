(ns aoc2024.day16
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            [aoc2024.clerk :as clrk]
            ))

(def array (au/adata (ju/file-lines "data/day16x.txt")))

;;; Correct but fails on p1 with stack overflow, sigh
(u/defn-memoized shortest-path
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
    (shortest-path arr start end nil nil)
    ))

;;; Try to just use last (not working for some reason)
(defn shortest-path-i
  [arr from dir to]
  (loop [[[this-p this-dir :as state] & rest] [[from dir]]
         distances {[from dir] 0}]
    (prn :s this-p this-dir distances)
    (if this-p
      (let [neighbors
            (->> (au/neighbors this-p)
                 (remove #(= \# (au/rget arr %))))
            this-cost (get distances state )
            neighbors-dir-cost (u/mapf (fn [n]
                                         (let [ndir (au/p- n this-p)
                                               cost (+ this-cost (if (= dir ndir) 1 1001))
                                               add?
                                               (if-let [prev-cost (get distances [n ndir])]
                                                 (< cost prev-cost)
                                                 true)]
                                           (when add? [[n ndir] cost])))
                                       neighbors)]
        (prn :p neighbors-dir-cost)
        (recur (concat rest (map first neighbors-dir-cost))
               (merge distances (into {} neighbors-dir-cost))))
      )))


(defn p1-i
  [arr]
  (let [start (au/afind arr \S)
        end (au/afind arr \E)]
    (shortest-path-i arr start [1 0] end)
    ))


#_
(p1 array)

