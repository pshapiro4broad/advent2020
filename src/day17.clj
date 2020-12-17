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
  (for [x (range (count (first input)))
        y (range (count input))
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

(defn all-neighbors [cubes]
  (->> (for [cube cubes
             near adjacent]
         (mapv + cube near))
       (into #{})))

(defn next-active [cubes cube]
  (let [active? (get cubes cube)
        active-neighbors  (->> adjacent
                               (map #(mapv + cube %))
                               (filter #(get cubes %))
                               count)]
    (if active?
      (or (= active-neighbors 3) (= active-neighbors 4))    ; 3, 4 here because we're also our neighbor
      (= active-neighbors 3))))

(defn next-cycle [cubes]
  (->> (all-neighbors cubes)
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