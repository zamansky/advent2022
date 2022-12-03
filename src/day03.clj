(ns day03
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]
   [clojure.set :as set]))

(def sample
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def data (slurp "data/day03.dat"))

(defn score [letter]
  (if (Character/isUpperCase letter)
    (+ 27 (-  (int letter) (int \A)))
    (inc (-  (int letter) (int \a)))
    ))




(defn parse-part1 [line]
  (let [size (/  (count line) 2)
        left (take size line)
        right (drop size line)
        common (set/intersection (set left) (set right))]
    (score (first common))))

(defn part1 [data]
  (->> (str/split data #"\n")
       (map  parse-part1)
       (reduce +)))
 
(part1 sample)
(part1 data)

(defn part2 [data]
  (->> (str/split data #"\n")
       (map set)
       (partition 3)
       (map  #(reduce set/intersection %))
       (map first)
       (map score)
       (reduce +)))

(part2 sample)
(part2 data)




