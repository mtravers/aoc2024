(ns aoc2024.day3
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            ))

(defn p1 []
  (as-> (slurp "data/day3.txt") data
    (re-seq #"mul\((\d*?),(\d*?)\)" data)
    (map (fn [[_ a b]]
           (* (u/coerce-numeric a) (u/coerce-numeric b)))
         data)
    (reduce + data)))

(defn p2 []
  (let [data (slurp "data/day3.txt")
        segs (cons
              (re-find #"^.*?don't\(\)" data)
              (map first (re-seq  #"do\(\)(.|\n)*?(don't\(\)|$)" data)))]
    (reduce +
            (map (fn [seg]
                   (as-> seg data
                     (re-seq #"mul\((\d*?),(\d*?)\)" data)
                     (map (fn [[_ a b]]
                            (* (u/coerce-numeric a) (u/coerce-numeric b)))
                          data)
                     (reduce + data)))
                 segs))))


