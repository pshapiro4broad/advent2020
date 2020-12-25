(ns day17
  (:require [clojure.string :as str]))

(def input
  (-> "src/day17-input.txt"
      slurp
      str/split-lines))

(def test-input
  '(".#." "..#" "###"))

(def occupied \#)

(defn parse-input [input]
  (for [x (range (count input))
        y (range (count (first input)))
        :when (= occupied (nth (nth input x) y))]
    [x y]))

(defn parse-input-3d [input]
  (->> (parse-input input)
       (map #(conj % 0))
       (into #{})))

(defn parse-input-4d [input]
  (->> (parse-input input)
       (map #(conj % 0 0))
       (into #{})))

(def adjacent
  (for [x [-1 0 1]
        y [-1 0 1]
        z [-1 0 1]
        w [-1 0 1]]
    [x y z w]))

(defn possible-active [cubes]
  (->> (for [cube cubes
             near adjacent]
         (mapv + cube near))
       distinct))

(defn active-neighbors [all-alive cell]
  (count
    (for [delta adjacent
          :let [neighbor (mapv + cell delta)]
          :when (get all-alive neighbor)]
      neighbor)))

(defn next-active [cubes cube]
  (let [active? (get cubes cube)
        active-neighbors (active-neighbors cubes cube)]
    (if active?
      (or (= active-neighbors 3) (= active-neighbors 4))    ; 3, 4 here because we're also our neighbor
      (= active-neighbors 3))))

(defn next-cycle [cubes]
  (->> (possible-active cubes)
       (filter #(next-active cubes %))
       (into #{})))

(defn run-conway [cycles cubes]
  (loop [cycles cycles
         cubes cubes]
    (if (zero? cycles)
      cubes
      (recur (dec cycles) (next-cycle cubes)))))

(defn part1 []
  (->> (parse-input-3d input)
       (run-conway 6)
       count))

(defn part2 []
  (->> (parse-input-4d input)
       (run-conway 6)
       count))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )