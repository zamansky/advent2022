(ns day06
    (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.walk :as walk]))

(def data (slurp "data/day06.dat"))
  
(defn part1 [line]
  (let [seq (->> (partition 4 1 line)
                 (map frequencies)
                 (map count)
                 (map-indexed (fn [idx item] [idx item]))
                 (drop-while (fn [[idx item]] (not= item 4))))]
    (+ 4 (first (first seq)))))
(defn part1 [line]
  (let [seq (->> (partition 4 1 line)
                 (map frequencies)
                 (map count)
                 (map-indexed (fn [idx item] [idx item]))
                 (drop-while (fn [[idx item]] (not= item 4))))]
    (+ 4 (first (first seq)))))

(defn solve [line length]
  (let [seq (->> (partition length 1 line)
                 (map frequencies)
                 (map count)
                 (map-indexed (fn [idx item] [idx item]))
                 (drop-while (fn [[idx item]] (not= item length))))]
    (+ length (first (first seq)))))

(solve data 4)
(solve data 14)
