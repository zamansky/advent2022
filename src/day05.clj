(ns day05
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.walk :as walk]))

(def raw-sample "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")

(comment

;; for the sample
  (def raw-data raw-sample)
  (def raw-stacks (first (str/split raw-data #"\n\n")))
  (def raw-moves (second (str/split raw-data #"\n\n")))

;; the real data
  (def raw-data (slurp "data/day05.dat"))
  (def raw-stacks (first (str/split raw-data #"\n\n")))
  (def raw-moves (second (str/split raw-data #"\n\n")))

)

(defn make-towers
  "Turn the text of the towers into a dictionary
  keys are tower names, list of vectors at each entry"
  [raw-stacks]
  (->> (str/split-lines raw-stacks)
       (apply mapv vector) ;; transpose the "matrix"
       (mapv reverse) ;; reverse them so that the tower names are  left
       (filterv (fn [x] (not= (first x) \space))) ;; remove non tower rows
       (mapv (fn [x] (filterv #(not= % \space) x))) ;; trim trailing space

       ;; turn list of lists into a dictionary
       (reduce (fn [acc next]
                 (assoc acc (first next) (into [] (rest next)))) {})))



(defn single-move
  "Make a single move from one tower to another"
  [[src dest] towers]
  (let [val (last (get towers src))
        towers (update towers src #(into [] (drop-last %)))
        towers   (update towers dest #(into [] (conj % val)))]
        towers) )

(defn multi-move
  "Call single-move multiple times to process a move statement"
  [[src dst count] towers]
  (loop  [towers towers count count]
    (if (= count 0)
      towers
      (recur (single-move [src dst] towers) (dec count) ) )))


(defn parse-move
  "move num from src to dst --> [src dest num]"
  [line]
  (let [[_ count src dst] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    [src dst (read-string count)])) ; note convert count to int


(def towers (make-towers raw-stacks))
(def m (map parse-move (str/split-lines raw-moves)))
(def part-1-ans-towers (reduce (fn [acc [src dst count]]
          (multi-move [(first src) (first dst) count] acc)) towers
          m))
(map (comp last second) part-1-ans-towers)

     
(defn move-part2 [[src dest count] towers]
  (let [val (into [] (take-last count (get towers src)))
        towers (update towers src #(into [] (drop-last count %)))
        towers   (update towers dest #(into [] (apply conj % val)))
        ]
        towers) )


(def part-2-ans
  (reduce (fn [acc [src dst count]]
            (move-part2 [(first src) (first dst) count] acc)) towers
          m))
(map (comp last second) part-2-ans)




