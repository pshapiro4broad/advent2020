(ns day12
  (:require [clojure.string :as str]
            [clojure.core.matrix.operators :refer :all]))

(def input
  (-> "src/day12-input.txt"
      slurp
      str/split-lines))

(def test-input '("F10" "N3" "F7" "R90" "F11"))

(defn compute-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x1 x2))
     (Math/abs ^int (- y1 y2))))

(def dir->delta
  {:N [-1 0] :S [1 0] :E [0 1] :W [0 -1]})

(defn turn->dir [turn size facing]
  (let [size (if (= turn :L) size (- 360 size))]
    (case size
      90 (case facing :E :N, :N :W, :W :S, :S :E)
      180 (case facing :E :W, :N :S, :W :E, :S :N)
      270 (case facing :E :S, :N :E, :W :N, :S :W))))

(defn decode [inst]
  [(keyword (subs inst 0 1))
   (read-string (subs inst 1))])

(defn part1 []
  (loop [input input
         facing :E
         ship [0 0]]
    (if (empty? input)
      (compute-distance [0 0] ship)
      (let [inst (decode (first input))
            dir (first inst)
            size (last inst)]
        (case dir
          (:L :R) (recur (next input) (turn->dir dir size facing) ship)
          (recur (next input) facing (+ ship (* size (dir->delta dir (dir->delta facing))))))))))

(defn rotate-by [turn size [x y]]
  (let [size (if (= turn :L) size (- 360 size))]
    (case size
      90 [(* -1 y) x]
      180 [(* -1 x) (* -1 y)]
      270 [y (* -1 x)])))

(defn part2 []
  (loop [input input
         ship [0 0]
         waypoint [-1 10]]
    (if (empty? input)
      (compute-distance [0 0] ship)
      (let [inst (decode (first input))
            dir (first inst)
            size (last inst)]
        (case dir
          (:L :R) (recur (next input) ship (rotate-by dir size waypoint))
          :F (recur (next input) (+ ship (* size waypoint)) waypoint)
          (recur (next input) ship (+ waypoint (* size (dir->delta dir)))))))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
