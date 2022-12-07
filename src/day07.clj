(ns day07
  (:require
   [utils :as u]
   [hashp.core]
   [clojure.string :as str]
   [com.rpl.specter :as sp]
   [clojure.set :as set]))


(def sample "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")


(defn update-state [{:keys [path listings dirs] :as state} line]
  (cond
    (= line "$ cd /") (assoc state :path ["/"])
    (= line "$ cd ..") (update state :path pop)
    (str/starts-with? line "$ cd") (update state :path conj (subs line 5))
    (= line "$ ls") state
    
    (str/starts-with? line "dir")
    (let [newdir (subs line 4)
          pname (str/join "/" path)
          d (get dirs pname [])
          d2 (conj d newdir)
          d3 (assoc dirs pname d2)]
      (assoc state :dirs d3))

    :else (let [[_ num _] (re-find #"^(\d+) (.+)" line)
                num (read-string num)
                pname (str/join "/" path)
                l (get listings pname [])
                l2 (conj l num)
                l3 (assoc listings pname l2)]
            (assoc state :listings l3))))



(defn sizes [node {:keys [listings dirs] :as state}]
  (let [l (get listings node)
        d (get dirs node)]
    (cond
      (nil? d) (apply + l)

      :else
      (let [paths  (map (fn [x] (format "%s/%s" node x)) d)
            psizes (map #(sizes % state) paths)
            r (apply + psizes)]
        (+ r (apply + l))))))

(sizes "/" state)

(def data (slurp "data/day07.dat"))


(def sample-state (reduce update-state
                   {:path [] :listings {}} (str/split-lines sample)))
(def state (reduce update-state
                   {:path [] :listings {}} (str/split-lines data)))

(defn part1 [state]
  (apply + (filter #(> 100000 %) (map #(sizes % state)
                                      (set/union (keys (:listings state))
                                                 (keys (:dirs state)))))))

(part1 state)


(def nodes
  (set/union (keys (:listings state))
             (keys (:dirs state))))

(def space-used) (sizes "/" staet)

(def free-space (- 70000000 space-used))
(def space-needed (-  30000000 free-space))

( def part2-ans
 (->> (map #(sizes % state ) nodes)
      (filter (fn [x] (>
                       (-  (+ x 70000000) (sizes "/" state)) 30000000)))
     (apply min)
     ))



