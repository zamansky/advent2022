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
move 1 from 1 to 2")



(def raw-stacks (first (str/split raw-sample #"\n\n")))
(def raw-moves (second (str/split raw-sample #"\n\n")))

(defn make-towers [raw-stacks] (->> (str/split-lines raw-stacks)
                                    (apply mapv vector)
                                    (mapv reverse)
                                    (filterv (fn [x] (not= (first x) \space)))
                                    (mapv (fn [x] (filterv #(not= % \space) x)))
                                    (reduce (fn [acc next]
                                              (assoc acc (first next) (into [] (rest next)))) {})))

(def towers (make-towers raw-stacks))
     
(defn single-move [[src dest] towers]
  (let [val (last (get towers src))
        towers (update towers src drop-last)
        towers (update towers dest conj val)
        ]
        towers)
  )

(defn multi-move [[src dst count] towers]
  (loop  [towers towers count count]
    (if (= count 0)
      towers
      (recur (single-move [src dst] towers) (dec count) ) )))

(defn parse-move [line]
  (let [[_ count src dst] (re-find #"move (\d) from (\d) to (\d)" line)]
    [src dst (read-string count)]))

(def ans (into (sorted-map)  (reduce (fn [acc [src dst count]]
          (multi-move [(first src) (first dst) count] acc)) towers
          (map parse-move (str/split-lines raw-moves))))))

(map (comp last second) ans)

(multi-move  (first (map parse-move (str/split-lines raw-moves)))towers )
(multi-move ["2 "1" 1]")


(def ans (part1 towers raw-moves))
ans

(single-move [\2 \1] towers)
(multi-move [\2 \3 3] towers)
(get  towers \1)

