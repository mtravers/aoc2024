(ns aoc2024.day15
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [aoc2024.utils :as au]
            ))

(def array (au/adata (ju/file-lines "data/day15-array.txt")))

(def moves "<^^>>>vv<v>>v<<")
(def moves (u/remove= \newline (slurp  "data/day15-moves.txt" )))

(defn v
  [ch]
  (case ch
    \< [-1 0]
    \> [1 0]
    \^ [0 -1]
    \v [0 1]
    ))


;;; Pos is O to slide
(defn slide
  [arr pos v]
  #_ (prn :slide pos v)
  (let [npos (au/p+ pos v)
        target (au/rget arr npos)]
    (case target
      \# nil
      \O (when-let [narr (slide arr npos v)]
           (au/rset narr pos \O))
      \. (au/rset arr npos \O))))

(defn move
  [arr pos moves]
  #_ (prn :p pos)
  #_ (au/print-array (au/rset arr pos \$))
  (if (empty? moves)
    arr
    (let [move (first moves)
          v (v move)
          npos (au/p+ pos v)
          target (au/rget arr npos)]
      (case target
        \# (recur arr pos (rest moves))
        \O (if-let [narr (slide arr npos v)]
             (recur (au/rset narr npos \.) npos (rest moves))
             (recur arr pos (rest moves)))
        \. (recur arr npos (rest moves))))))

(defn p1
  []
  (let [start (au/afind array \@)
        final (move (au/rset array start \.) start moves)
        pts (au/afind-all final \O)
        ]
    (reduce + (map (fn [[x y]] (+ x (* y 100))) pts))))

;;; P2

(defn expand-arr
  [arr]
  (mapv (fn [line]
            (vec (mapcat (fn [c]
                           (case c
                             \# "##"
                             \O "[]"
                             \. ".."
                             \@ "@."
                             ))
                         line)))
          arr
          ))


;;; Pos is [ or ] to slide
(defn slide2-h
  [arr pos v prev]
  (let [this (au/rget arr pos)          ;piece ro move
        npos (au/p+ pos v)              ;dest
        target (au/rget arr npos)]      ;piece to move over
    #_ (prn :slide-h pos v prev this target )
    (case target
      \# nil
      (\[ \]) (when-let [narr (slide2-h arr npos v this)] ;when we could do the move
                (au/rset narr pos prev))
      \. (-> arr
             (au/rset npos this)
             (au/rset pos prev)))))

(defn box-other
  [array [x y]]
  (case (au/rget array x y)
    \[ [(inc x) y]
    \] [(dec x) y]))

(declare slide2-v)

;;; Try to slide one half of a block up or down
(defn slide2-va
  [arr pos v prev]
  #_ (prn :slide-va pos v prev)
  (let [npos (au/p+ pos v)
        this (au/rget arr pos)
        target (au/rget arr npos)]
    (case target
      \# nil
      (\[ \]) (when-let [narr (slide2-v arr npos v this)]
                (au/rset narr pos \.))
      \. (-> arr
             (au/rset npos this)
             (au/rset pos \.)))))

(defn slide2-v
  [arr pos v prev]
  #_ (prn :slide-v pos v)
  (let [other (box-other arr pos)]
    (when-let [narr (slide2-va arr pos v prev)]
      (when-let [narr2 (slide2-va narr other v prev)]
        #_ (prn :foo pos other prev)
        (au/rset narr2 pos prev)
        #_ (au/rset narr2 other \.)))))

(defn slide2
  [arr pos v]
  (if (zero? (first v))
    (slide2-v arr pos v \.)
    (slide2-h arr pos v \.)))

;;; → Multitool! Can't believe I had to write this
(defn subseq?
  [ss s]
  (some #(= ss %) (partition (count ss) 1 s)))

(defn bad-array?
  [arr]
  (or (some #(subseq? '(\] \]) %) arr)
      (some #(subseq? '(\[ \[) %) arr)))

(defn move2
  [arr pos moves]
  (prn :p pos (first moves))
  #_ (au/print-array (au/rset arr pos \$))
  (when (bad-array? arr) (throw (ex-info "BAD" {:p pos})))
  (if (empty? moves)
    arr
    (let [move (first moves)
          v (v move)
          npos (au/p+ pos v)
          target (au/rget arr npos)]
      (case target
        \# (recur arr pos (rest moves))
        (\[ \])
        (if-let [narr (slide2 arr npos v)]
             (recur (au/rset narr npos \.) npos (rest moves))
             (recur arr pos (rest moves)))
        \. (recur arr npos (rest moves))))))

(def array (au/adata (ju/file-lines "data/day15-arr-p2x.txt")))
(def moves "<vv<<^^<<^^")
(def moves "<^<<<<vv<<^^")

;;; YES this reproduces the bug
(def array (au/adata (ju/file-lines "data/day15-arr-p2y.txt")))
(def moves "<<v<>^^<<<<vv<<^^")

(defn p2
  []
  (let [array (expand-arr array)
        start (au/afind array \@)
        final (move2 (au/rset array start \.) start moves)
        pts (au/afind-all final \[)
        ]
    (au/print-array final)
    (reduce + (map (fn [[x y]] (+ x (* y 100))) pts))))

;; too high 1647378
