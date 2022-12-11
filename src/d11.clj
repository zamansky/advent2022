(ns day11
  (:require
   [clojure.string :as str]
))



(def sample (slurp "../data/sample11.dat"))


(def data (slurp "../data/day11.dat"))

(def raw-list (str/split data #"\n\n"))

(def opmap {"+" + "*" *})
(defn parse-ape [monkey]
  (let [[n a b c d e] (str/split-lines monkey)
        n (read-string (re-find #"\d" n))
        items (mapv read-string (re-seq #"-?\d+" a))
        [_ left op right] (re-find #"(old) (.) (-?\d+|old)" b)
        right (if (= right "old") right (read-string right))
        test (read-string (re-find #"\d+" c))
        true-ape (read-string (re-find #"\d+" d))
        false-ape (read-string (re-find #"\d+" e))]

    {:number n
     :items items
     :op (list (get opmap op) left right)
     :test test
     :true-ape true-ape
     :false-ape false-ape
     :counts 0}))

(def apes  (mapv parse-ape raw-list))

(defn do-op [val oplist]
  (let [oplist (map (fn [x] (if (= x "old") val x)) oplist)
        
        ]
    (quot  (eval  oplist) 3)
    ))

(defn process-ape [apes number]
  (reduce (fn [apes item]
            (let [item (do-op item (get-in apes [number :op]))
                  apes (update-in apes [number :items] #(subvec % 1))
                  apes (update-in apes [number :counts] inc)]
              (if (= (mod item (get-in apes [number :test])) 0)
                (update-in apes [(get-in apes [number :true-ape]) :items] conj item)
                (update-in apes [(get-in apes [number :false-ape]) :items] conj item))))

          apes (get-in apes [number :items])))


(defn turn [apes]
  (reduce process-ape apes (range (inc (count apes)))))

(defn part1 [apes]
  (->>
        (iterate turn apes)
        (drop 20)
        first
        (map :counts)
        sort
        reverse
        (take 2)
        (apply *)
        ))

(part1 apes)

(defn do-op-pt2 [val oplist]
  (let [oplist (map (fn [x] (if (= x "old") val x)) oplist)
        reducer (reduce * (map :test apes))
        ]
    (rem  (eval  oplist) reducer)
    ))

(defn process-ape-pt2 [apes number]
  (reduce (fn [apes item]
            (let [item (do-op-pt2 item (get-in apes [number :op]))
                  apes (update-in apes [number :items] #(subvec % 1))
                  apes (update-in apes [number :counts] inc)]
              (if (= (mod item (get-in apes [number :test])) 0)
                (update-in apes [(get-in apes [number :true-ape]) :items] conj item)
                (update-in apes [(get-in apes [number :false-ape]) :items] conj item))))

          apes (get-in apes [number :items])))


(process-ape-pt2 apes 0)
(def papt2 (memoize process-ape-pt2))

(defn turn-pt2 [apes]
  (reduce  papt2 apes (range (inc (count apes)))))

(def tpt2 (memoize turn-pt2))





(defn part2 [times apes]
  (->>
        (iterate tpt2 apes)
        (drop times)
        first
        (map :counts)
        sort
        reverse
        (take 2)
        (apply *)
        ))

(def z (part2 10000 apes))
(println z)



