(ns aoc2024.day8
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def data 
  (au/adata (ju/file-lines "data/day8.txt")))

(def p+ (u/vectorize +))
(def p- (u/vectorize -))

(defn nodes
  [p1 p2]
  (filter (partial au/in-bounds? data)
          [(p- p1 (p- p2 p1))
           (p- p2 (p- p1 p2))]))

(defn more-nodes
  [p1 p2]
  (concat (take-while (partial au/in-bounds? data) (iterate (partial p+ (p- p2 p1)) p1))
          (take-while (partial au/in-bounds? data) (iterate (partial p+ (p- p1 p2)) p1))))

(defn all-nodes
  [nodef]
  (let [chs
        (dissoc (group-by #(apply au/rget data %) (au/all-points data)) \.)
        beacons
        (reduce
         concat
         (for [g (vals chs)
               [b1 & rst] (tails g)
               b2 rst]
           (nodef b1 b2)))]
    (count 
     (distinct beacons))))

(defn p1
  []
  (all-nodes nodes))

(defn p2
  []
  (all-nodes more-nodes))

        

          
