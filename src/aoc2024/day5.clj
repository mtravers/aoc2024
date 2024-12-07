(ns aoc2024.day5
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))



(defn get-data
  []
  (let [lines (ju/file-lines "data/day5.txt")
        [a b] (split-with (comp not empty?) lines)
        rules (set (map #(split-nums % #"\|") a))
        prints (map #(split-nums % #",") (rest b))
        ]
    [rules prints]))

(defn seq-pairs
  [seq]
  (when (> (count seq) 1)
    (concat (map (fn [e] [e (first seq)]) (rest seq))
          (seq-pairs (rest seq)))))

(defn seq-ok
  [seq rules]
  (every? #(not (contains? rules %)) (seq-pairs seq)))

(defn middle-elt
  [s]
  (nth s (/ (count s) 2)))

(defn p1
  []
  (let [[rules seqs] (get-data)
        valid (filter #(seq-ok % rules) seqs)]
    (reduce + (map middle-elt valid))))

(defn seq-violations
  [seq rules]
  (filter #(contains? rules %) (seq-pairs seq)))

(defn topo-sort
  [rules seq]
  (if (empty? (rest seq))
    seq
    (let [end (u/some-thing (fn [elt] (not (some (fn [other] (contains? rules [elt other])) seq)))
                            seq)]
      (conj (vec (topo-sort rules (u/remove= end seq))) end))))

(defn p2
  []
  (let [[rules seqs] (get-data)
        invalid (remove #(seq-ok % rules) seqs)]
    (reduce +
            (map (comp middle-elt (partial topo-sort rules)) invalid))))
