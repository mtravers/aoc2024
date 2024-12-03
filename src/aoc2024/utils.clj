(ns aoc2024.utils
  (:require [clojure.string :as s]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  )

(defn split-tokens
  [s]
  (s/split s #" +"))

(defn split-tokens-numeric
  [s]
  (map u/coerce-numeric (split-tokens s)))
