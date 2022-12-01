(ns day01
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
[clojure.java.io :as io]))


(def sample "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")


(def raw-data (slurp "data/day01.dat"))

(defn process [data]
  (->> (str/split data #"\n\n")
       (map #(str/split % #"\n"))
       (sp/transform [sp/ALL sp/ALL] utils/parse-int)
       (map  #(apply + %))
       (sort)
       (reverse)
       ))

(process sample)

(defn part1 [data]
  (-> data
      process
      first))

(defn part2 [data]
  (->> data
       process
       (take 3)
       (apply +)))






