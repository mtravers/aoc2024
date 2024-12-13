(ns aoc2024.day13
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

;;; Algebraic!

(def data (->> "data/day13.txt"
                ju/file-lines
                (partition-by empty?)
                (remove #(= 1 (count %)))
                (map #(apply str %))
                (map #(re-seq #"\d+" %))
                (map (u/vectorize u/coerce-numeric))))

(defn solve
  [ax ay bx by px py]
  (let [a (/ (- (* bx py) (* by px))
             (- (* bx ay) (* by ax)))
        b (/ (- px (* ax a)) bx)]
    (when (and (pos? a) (pos? b)
               (int? a) (int? b))
      (+ (* 3 a) b))))

(defn p1
  []
  (reduce + (u/mapf #(apply solve %) data)))

(defn p2x
  [data]
  (-> data
      (update 4 + 10000000000000)
      (update 5 + 10000000000000)))
      
(defn p2
  []
  (reduce + (u/mapf #(apply solve %) (map p2x data)))  )
