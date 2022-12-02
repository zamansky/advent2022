(ns day02
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]))


(def sample "A Y
B X
C Z")

;; x = 0
;; y = 1
;; z = 2
;; 0 = lost
;; 3 = tie
;; 6 = win

(def data (slurp "data/day02.dat"))

(def values {"X" 1 "Y" 2 "Z" 3 "A" 1 "B" 2 "C" 3})
(def scores {-2 6 -1 0 0 3 1 6 2 0})

(defn parse-part1 [line]
  (let [[a b] (str/split line #" ")
        index (- (get values b) (get values a))
        score (+ (get values b) (get scores index))
        ]
  score))

(parse-part1 "A Y")


(defn part1 [data]
  (->> (str/split data #"\n")
       (map parse-part1)
       (apply +)))
;; X = you have to lose
;; Y have to draw
;; Z have to win

(def moves ["A" "B" "C"])
(def moveindex {"A" 0 "B" 1 "C" 2 })

(defn parse-part2 [line]
  (let [[a b] (str/split line #" ")
        mi  (get moveindex a)
        b (cond (= b "X") (get moves (mod  (+ 2  mi)3 ))
                (= b "Y") (get moves mi)
                (= b "Z") (get moves (mod (+ 1  mi) 3)))
        
        index (- (get values b) (get values a))
        score (+ (get values b) (get scores index))]
    score))


(parse-part2 "A Z")
(defn part2 [data]
  (->> (str/split data #"\n")
       (map parse-part2)
       (apply +)))
(part2 sample)

(part2 data)
