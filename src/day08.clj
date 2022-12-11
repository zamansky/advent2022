(ns day08
  (:require
      [utils :as u]
[clojure.string :as str]
   [clojure.set :as set]
      [hashp.core]
      [clojure.pprint]
      ))

(def sample
"30373
25512
65332
33549
35390")


(def data (slurp "data/day08.dat"))

;; 5 would be visible because every tree to edge from it
;; is less than it
(every? #(< % 5) [3 2 2])


(defn transpose [m]
  (apply mapv vector m))


(defn lines->lol
  [lines]
   (->> (str/split-lines lines)
        (mapv (fn [x] (mapv #(Character/digit % 10) x )))
        ))

(defn is-tree-visible? [row col grid tgrid]
  (let [val (get-in grid [row col])]
    (or
     (every? #(< % val)  (subvec (grid row) (inc col)))
     (every? #(< % val) (rseq (subvec (grid row) 0 col)))
     (every? #(< % val)  (subvec  (tgrid col) (inc row)))
     (every? #(< % val)   (rseq (subvec (tgrid col) 0  row))))))


(defn visible [data]
  (let [grid (lines->lol data)
        tgrid (transpose grid)
        width (count grid)
        height width]
    (for [row (range 1   (- height 1))  col (range 1  (- width 1))]
      (is-tree-visible? row col grid tgrid))))

(defn count-trees [val vec]
  (let [smaller (take-while #(< % val) vec)
        others (drop-while  #(< % val) vec)
        ]
    (if (empty? others)
      (count smaller)
      (inc (count smaller)))))

(count-trees 5 [1 2])

(defn count-visible [row col grid tgrid]
  (let [val (get-in grid [row col])]
    (*
      (count-trees val (subvec (grid row) (inc col)))
      (count-trees val (rseq (subvec (grid row) 0 col)))
      (count-trees val (subvec  (tgrid col) (inc row)))
      (count-trees val (rseq (subvec (tgrid col) 0  row))))))



(defn part2 [data]
  (let [grid (lines->lol data)
        tgrid (transpose grid)
        width (count grid)
        height width]
    (for [row (range 1   (- height 1))  col (range 1  (- width 1))]
      (count-visible row col grid tgrid))))

(apply max (part2 data))



(count-visible 1 2 grid tgrid)
(take-while #(< % 5) [3 5 3])

(+ 5 5 3 3 (count (filter true? (visible sample))))
(+ 99 99 97 97  (count (filter true? (visible data))))

(visible sample)

;; 1671 too low I think it's 1681

(subvec g 1 )

(def grid (lines->lol sample))
(def tgrid (transpose grid))

(clojure.pprint/pprint tgrid)
