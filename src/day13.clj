(ns day13
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]
   [clojure.edn :as edn]))

(defn parse [f]
  (->> (str "[" (slurp f) "]")
       edn/read-string
       ))



(def sample (parse "data/sample13.dat"))
(def data (parse "data/day13.dat"))



(defn compare-lists [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (recur [a] b)
    (number? b) (recur a [b])
    :else  (or  (->> (map compare-lists a b) (drop-while zero?) first)
                (- (count a) (count b))
                )))

(defn compare-pair [[a b]]
  (compare-lists a b)
  )


(defn part1 [data]
  (->>
   (map compare-pair data)
   (keep-indexed (fn [i v] (when (< v 0) (inc i))))
   (reduce +)))

(part1 (partition 2 sample))

(keep-indexed (fn [i v] (when (< v 0) (inc i))) (part1 sample))
(part1 (partition 2 data))

(def look-for #{[[2]] [[6]]})

(defn part2 [data]
  (->> data
   (concat look-for)
   (sort compare-lists)
   (keep-indexed (fn [i v] (when (look-for v) (inc i))))
   (reduce *)
   )
  )
 

(part2 sample)
(part2 data)
