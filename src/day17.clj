(ns day17
  (:require [clojure.string :as str]))

(def input
  (-> "src/day17-input.txt"
      slurp
      str/split-lines))

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

(defn neighborhood-3d [[x y z]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]
        :when (not= 0 dx dy dz)]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn next-cycle [get-neighbors cubes]
  (set (for [[cube count] (frequencies (mapcat get-neighbors cubes))
             :when (or (= count 3)
                       (and (cubes cube) (= count 2)))]
         cube)))

(defn part1 []
  (->> (parse-input-3d input)
       (iterate (partial next-cycle neighborhood-3d))
       (drop 6)
       first
       count))

(defn neighborhood-4d [[x y z w]]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw [-1 0 1]
        :when (not= 0 dx dy dz dw)]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn part2 []
  (->> (parse-input-4d input)
       (iterate (partial next-cycle neighborhood-4d))
       (drop 6)
       first
       count))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )