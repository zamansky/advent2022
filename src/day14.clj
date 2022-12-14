(ns day14
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]
   [clojure.edn :as edn]))


(def data (u/load-data 2022 14))

(def sample "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parse-line [line]
  (let [p1  (map #(str/split % #",") line )
        p2 (map (fn [x] (map read-string x)) p1)
        ]p2))


(defn add-line [world [x0 y0] [x1 y1]]
  (let [[x0 x1] (sort [x0 x1])
        [y0 y1] (sort [y0 y1])
        entries (if (= x0 x1)
                  (for [  y (range y0 (inc y1))]
                    (assoc world [ x1  y] \#))
                  (for [x (range x0 (inc x1))]
                    (assoc world [#p x #p y1] \#))
                  
                  )

        ]

    (into {} entries)))

(defn add-lines [world lines]
  (reduce (fn [world [p0 p1]]
          (add-line world  p0  p1)
          ) world lines
          ))

(defn build-world [data]
  (let [line-set (->> (str/split-lines data)
                      (map  #(str/split % #" -> "))
                      (map parse-line)
                      (map #(partition 2 1 %)))]

    (reduce (fn [world lines]
              (add-lines world lines)
              ) {} line-set)
    ))




(def sample-world (build-world sample))

(def world (build-world data))


(defn drop-sand [x y [min-x max-x min-y max-y :as bounds] world]
  (cond (or (> y max-y)
            (< x min-x)
            (> x max-x)) world
        (nil? (world [x (inc y)])) (recur x (inc y) bounds world)
        (nil? (world [(dec x) (inc y)])) (recur (dec x) (inc y) bounds world)
        (nil? (world [(inc x) (inc y)])) (recur (inc x) (inc y) bounds world)
          :else (assoc world [x y] \O)))



(defn part1 [world]
  (let [max-y (apply max (map second (keys world)))
        min-y (apply min (map second (keys world)))
        max-x (apply max (map first  (keys world)))
        min-x (apply min (map first  (keys world)))
        bounds [min-x max-x min-y max-y]]
    (loop [world world times 0]
      (let [nextworld (drop-sand 500 0 bounds world)]
        (if (= world nextworld) times
            (recur nextworld (inc times)))))))

(defn blocked? [x y world]
   (not (or (nil? (world [x (inc y)]))
       (nil? (world [(dec x) (inc y)]))
       (nil? (world [(inc x) (inc y)]))
)))

(defn drop-sand-pt2 [x y [min-x max-x min-y max-y :as bounds] world]
  (cond
    (and (blocked? x y world) (= y 0) (= x 500)) world
    (>= y (+ 1 max-y)) (assoc world [x y] \O)
    (nil? (world [x (inc y)])) (recur x (inc y) bounds world)
    (nil? (world [(dec x) (inc y)])) (recur (dec x) (inc y) bounds world)
    (nil? (world [(inc x) (inc y)])) (recur (inc x) (inc y) bounds world)
    :else  (assoc world [x y] \O)))

(defn part2 [world]
  (let [max-y (apply max (map second (keys world)))
        min-y (apply min (map second (keys world)))
        max-x (apply max (map first  (keys world)))
        min-x (apply min (map first  (keys world)))
        bounds [min-x max-x min-y max-y]]
    (loop [world world times 0]
      (let [nextworld (drop-sand-pt2 500 0 bounds world)]
        (if (= world nextworld) (inc times)
            (recur nextworld (inc times)))))))


(part1 world)

(part2 world)
