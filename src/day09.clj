(ns day09
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]))


(def state {:head [0 0] :tail [0 0] :visited {}})




(def movemap {\U [0 1] \D [0 -1] \L [-1 0] \R [1 0]})

(defn move-head [{:keys [head] :as state} dir]
  (update state :head #(mapv + (get movemap dir )%))
  )


defn connected? [{:keys [head tail] :as state}]
  (let [[x0 y0] head
        [x1 y1] tail]
    (and (<= (- x0 x1) 1)
         (<= (- y0 y1) 1))))

(defn p2connected? [head tail]
  (let [[x0 y0] head
        [x1 y1] tail]
    (and (<= (abs (- x0 x1)) 1)
         (<= (abs (- y0 y1)) 1))))



(def tailmoves
  {[1 -2]  [1 -1]
   [1 2] [1 1]
   [-1 -2] [-1 -1]
   [-1 2] [-1 1]
   [2 -1] [1 -1]
   [2 1] [1 1]
   [-2 -1] [-1 -1]
   [-2 1] [-1 1]
   [0 2] [0 1]
   [0 -2] [0 -1]
   [2 0] [1 0]
   [-2 0] [-1 0]
   [2 2] [1 1]
   [2 -2] [1 -1]
   [-2 2] [-1 1]
   [-2 -2] [-1 -1]
   })



(defn move-tail [{:keys [head tail visited] :as state}]
   (if (connected? state)
     (assoc-in state [:visited tail] true)
    (let [diffs  (map - head tail)
          ]
      (-> state
          (assoc :tail (map + tail (get tailmoves diffs)))
                    (assoc-in  [:visited (map + tail (get tailmoves diffs))] true)
          (assoc-in  [:visited tail] true)
          )
      
      )))



(defn move-both [{:keys [head tail visited]:as state} dir]
   (-> state
      (move-head dir)
      move-tail
      ))


(defn make-moves [{:keys [head tail visited] :as state} dir steps]
  (reduce (fn [state dir]
            (-> state
                (move-head dir)
                move-tail))
          state (repeat steps dir))
  )

(def sample
  (str/split-lines
   "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"))

(def sample2
(str/split-lines "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"))

(defn parse [line]
  (let [[ds cs] (str/split line #" ")
        dir (get ds 0)
        steps (read-string cs)
        ]
    [dir steps]))


(def data (str/split-lines (slurp "data/day09.dat")))
(def instructions (map parse  data))

(def z (reduce (fn [state [dir steps]]
                (make-moves state dir steps))
               state
               instructions))

(count (:visited z))



(defn part1 [moves state ]
  (reduce (fn [state [dir steps]]
                 (make-moves state dir steps))
               state
               moves))

(part1 instructions state)q


(def p2state {:rope (apply vector (repeat 11 [0 0])) :visited {}})

(defn p2-move-head [{:keys [rope] :as state} dir]
  (let [newhead  (mapv + (get movemap dir) (first rope))
        newrope (assoc  (apply vector rope) 0  newhead)]
    (assoc state :rope newrope)
    ))


(defn p2-move-tail [ state head-index tail-index]
  (let [head (get-in  state [:rope  head-index] )
        tail (get-in state [:rope  tail-index] )]
    (if (p2connected?  head tail)
      ;;(assoc-in state [:visited tail] (max tail-index (get-in state [:visible tail] 0)))
      state
      (let [diffs  (mapv - head tail)
            newtail (mapv + tail (get tailmoves diffs ))]
        (-> state
            (assoc-in [:rope tail-index]  newtail)
            (assoc-in  [:visited newtail] (max tail-index (get-in state [:visited newtail] 0) ))
            (assoc-in  [:visited tail] (max  tail-index  (get-in state [:visited tail] 0)))
            )))))

(defn p2-move-tails [ state ]
  (reduce (fn [state next]
            (p2-move-tail state  next (inc next))) state (range 9)))

(defn p2-make-moves [state dir steps]
  (reduce (fn [state dir]
            (-> state
                (p2-move-head dir)
                p2-move-tails)
            ) state (repeat steps dir)))


(defn part2 [moves state ]
  (reduce (fn [state [dir steps]]
                 (p2-make-moves state dir steps))
               state
               moves))

(count  (filter (fn [[k v]] (= v 9) ) (:visited (part2 instructions p2state))))










