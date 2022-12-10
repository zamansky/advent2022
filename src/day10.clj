(ns day10
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]))

(def sample1 (str/split-lines (slurp "data/day10-sample1.dat")))
(def sample2 (str/split-lines  (slurp "data/day10-sample2.dat")))
(def data (str/split-lines  (slurp "data/day10.dat")))

(defn part1-calc-states [lines]
  (reduce (fn [acc next]
                      (let [[op amt] (str/split next #" ")
                            amt (u/parse-int amt)
                            lastval (last acc)]
                        (if (= op "noop")
                          (conj acc lastval)
                          (-> acc
                              (conj lastval)
                              (conj (+ lastval amt)))))) [1] lines))
(defn part1 [data]
  (let [states (part1-calc-states data)]
    (reduce + (map #(* % (get states (dec %))) [20 60 100 140 180 220]))))

(part1 data)

(defn part2 [data]
  (let [states (part1-calc-states data)
        chars (reduce (fn [screen next]
                        (let [val (get states next)
                              idx (mod next 40 )]
                          (if (and (<= (dec val) idx) (>= (inc val) idx))
                            (str screen "#")
                            (str screen " ")))) "" (range 240))
        screen-lines (map #(apply str %) (partition 40 chars))
        ]
    screen-lines
    ))

(map println (part2 data))
