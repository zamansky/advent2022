(ns day12
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]
   [loom.graph :as g]
   [loom.alg :as alg]
   [loom.io :as vg]
   ))

(def sample
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")


(defn make-graph [data]
  (->> (str/split-lines data)
       (map-indexed  #(map-indexed (fn [k v] [%1 k v]) %2))
       (reduce (fn [acc next]
                 (concat acc next)) [])
       (reduce (fn [acc [r c v]]
                 (assoc acc [r c] {:loc [r c]
                                   :height (- (int v) (int \a))
                                   :edges []})) {})))

(def *start*  -14)
(def *exit* -28)

(def deltas [[1 0] [-1 0] [0 1] [0 -1]])

(defn is-neighbor? [current coords nodes]
  (let [potential (nodes coords)
        current-height  (:height current)
        current-height (if (= current-height -14) 25 current-height)
        current-height (if (= current-height -28) 25 current-height)
        pot-height (:height potential 100)
        pot-height (if (= pot-height *start*) 25 pot-height)
        pot-height (if (= pot-height *exit*) 25 pot-height)]
    (<=   (dec pot-height) current-height)))

(defn find-neighbors [k nodes]
  (let [potential  (mapv #(mapv + k %) deltas)
        current (nodes k)]
    (filter #(is-neighbor? current % nodes) potential)))

(find-neighbors [0 0] z)

(defn add-edges [graph] 
  (reduce (fn [nodes next]
            (assoc-in nodes [next :edges] (find-neighbors next nodes))) graph (keys graph)))


(defn make-edged-graph [data] (-> data
                                  make-graph
                                  add-edges))

(def data(slurp "data/day12.dat"))

(def z (make-edged-graph data))

;; find start and end coords
(def start-loc  (filter (fn [x] (= (get-in z [x :height]) *start*)) (keys z)))
(def end-loc (filter (fn [x] (= (get-in z [x :height]) *exit*)) (keys z)))

(defn make-loom-graph [graph]
  (reduce (fn [acc next]
            (assoc acc
                   (:loc (graph next))
                   (:edges (graph next))))
          {} (keys graph)))


;; 447 
(-> (make-edged-graph data)
    make-loom-graph
    g/digraph
    (alg/bf-path [20 0] [20 135])
    count
    dec
    )

;; 31 
(-> (make-edged-graph sample)
    make-loom-graph
    g/digraph
    (alg/bf-path [0 0] [2 5])
    count
    dec
    )



(def g (make-edged-graph data))

;; find all the a locations
(def acoords (filter (fn [[k v]]
                       (=  (:height v) 0)) g))

;; make the loom graph
(def gg (-> (make-edged-graph data)
    make-loom-graph
    g/digraph
    ))

;; find path from each a to exit, answer will be one less than
;; smallest non-zero
;; it would have been faster to search from exit to first a but
;; I don't know how to do that with the loom library
(drop-while #(= 0 %) (sort (map #(count (alg/bf-path gg % [20 135])) (keys acoords))))

