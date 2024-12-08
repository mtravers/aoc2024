(ns aoc2024.day7
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def data 
  (map (partial au/split-nums #"\D+")
       (ju/file-lines "data/day7.txt")))

(defn works?
  ([v a & rst]
   (if rst
     (or (apply works? v (* a (first rst)) (rest rst))
         (apply works? v (+ a (first rst)) (rest rst)))
     (= v a)))
  ([x]
   (apply works? x)))

(defn p1
  []
  (reduce + (map first (filter works? data))))

(defn works2?
  ([v a & rst]
   (if rst
     (or (apply works2? v (* a (first rst)) (rest rst))
         (apply works2? v (+ a (first rst)) (rest rst))
         (apply works2? v (au/ncat a (first rst)) (rest rst)))
     (= v a)))
  ([x]
   (apply works2? x)))

(defn p2
  []
  (reduce + (map first (filter works2? data))))
