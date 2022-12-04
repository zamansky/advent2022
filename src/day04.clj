(ns day04
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]
   [clojure.set :as set]))

(def sample
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
")

(def data (slurp "data/day04.dat"))

(defn parse [line]
  (let [[_ a0 a1 b0 b1]
        (re-find  #"(\d+)\-(\d+),(\d+)\-(\d+)" line)]
    [a0 a1 b0 b1]))

(parse "2-3,4-5")


(defn part1-check [[a0 a1 b0 b1]]
(or   (and (<= a1 b1) (>= a0 b0))
  (and (<= b1 a1) (>= b0 a0)))
  )

(defn part2-check [[a0 a1 b0 b1]]
 (not (or (> b0 a1) (< b1 a0)))
  )

(defn part1 [data]
  (->> (str/split-lines data)
       (map parse)
       (sp/transform [sp/ALL sp/ALL] utils/parse-int)
       (map part1-check)
       (filter true?)
       count))

(defn part2 [data]
  (->> (str/split-lines data)
       (map parse)
       (sp/transform [sp/ALL sp/ALL] utils/parse-int)
       (map part2-check)
       (filter true?)
       count))

(part1 sample)
(part1 data)
(part2 sample)
(part2 data)
