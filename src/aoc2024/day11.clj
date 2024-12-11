(ns aoc2024.day11
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def data [125 17])
(def data [510613 358 84 40702 4373582 2 0 1584])

(defn even-digits?
  [i]
  (even? (count (str i))))              ;lame

(defn split-even
  [i]
  (let [s (str i)
        m (/ (count s) 2)]
    (mapv u/coerce-numeric [(subs s 0 m) (subs s m)])))

(u/defn-memoized xform-stone
  [s]
  (cond (= 0 s) 1
        (even-digits? s) (split-even s)
        :else (* s 2024)))

(defn xform-seq
  [data]
  (flatten (mapv xform-stone data)))

(defn p1
  [data]
  (count (last (take 26 (iterate xform-seq data)))))

;; Nope, too slow
(defn p2
  [data]
  (count (last (take 76 (iterate xform-seq data)))))

;;; Smarter algo

(u/defn-memoized expand-count
  [i stone]
  (cond (= i 0) 1
        (= stone 0) (expand-count (dec i) 1)
        (even-digits? stone) (reduce +
                                     (map (partial expand-count (dec i))
                                          (split-even stone)))
        :else (expand-count (dec i) (* stone 2024))))

(defn p1a
  [data]
  (reduce + (map (partial expand-count 25) data)))

(defn p2a
  [data]
  (reduce + (map (partial expand-count 75) data)))
