(ns aoc2024.day18
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            [aoc2024.clerk :as clrk]
            ))

(defn solve
  [data grid-size]
  (let [grid (reduce (fn [grid p] (au/rset grid p \O))
                     (au/blank-array grid-size grid-size)
                     data)
        end [(dec grid-size) (dec grid-size)]]
    #_ (au/print-array grid)
    (loop [[p & fringe] [[0 0]]
           costs {[0 0] 0}]
      (let [ccost (inc (get costs p))
            neighbors (->> (au/neighbors p)
                           (filter (partial au/in-bounds? grid))
                           (remove #(= \O (au/rget grid %)))
                           (remove #(>= ccost (get costs % 9999999)))        ; if the already-seen cost is absent or > then current cost to fringe
                           sort
                           reverse
                           )]
        #_ (when (= 0 (rand-int 1000))
          (prn :p p neighbors)
          (au/print-array (au/rset (au/rset* grid (keys costs) \*) p \■ ))
          (prn))
        (cond (u/position= end neighbors)
              ccost
              (and (empty? neighbors) (empty? fringe))
              (do
                (au/print-array (au/rset* grid (keys costs) \*))
                (throw (ex-info "poop" {})))
              :else
              (recur (concat fringe neighbors)
                     (merge costs (zipmap neighbors (repeat ccost)))))))))

(defn read-data [f] (map au/split-nums (ju/file-lines f)))

(defn p1x
  []
  (solve (take 12 (read-data  "data/day18x.txt")) 7))

(defn p1
  []
  (solve (take 1024 (read-data "data/day18.txt")) 71))

;;; Works, the answer is going to by the rth datapoint given the last value of r
(defn p2-naive
  []
  (let [data (read-data "data/day18.txt")]
    (doseq [r (range 1024 (count data))]
      (prn :r r (solve (take r data) 71)))))
        
