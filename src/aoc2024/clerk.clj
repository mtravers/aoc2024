(ns aoc2024.clerk
  (:require 
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [nextjournal.clerk :as clerk]
            ))

;;; Clerking it

#_
(clerk/serve! {:browse? true :port 6611
               :watch-paths ["aoc2024" "src"]})

(defn cd
  [data]
  (clerk/html
   [:table {:style {:width "initial"}}
    (for [row data]
      [:tr
       (for [col row]
         [:td {:style {:border "1px solid lightgray" :text-align "center"  :width "25px" :height "25px" :font-weight "bold"}}
          (str col)])])]))



(def side-clerk (atom nil))

(defn cd-side
  [thing]
  (swap! side-clerk conj thing))

(defn with-side-clerk
  [f]
  (reset! side-clerk [])
  (f)
  (clerk/html `[:div ~@@side-clerk]))


