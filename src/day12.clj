(ns day12
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

(def input
  (-> "src/day12-input.txt"
      slurp
      str/split-lines))

(defn compute-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x1 x2))
     (Math/abs ^int (- y1 y2))))

(def dir->delta
  {:N [-1 0] :S [1 0] :E [0 1] :W [0 -1]})

(defn turn->dir [turn angle facing]
  (case (if (= turn :L) angle (- 360 angle))
    90 (case facing :E :N, :N :W, :W :S, :S :E)
    180 (case facing :E :W, :N :S, :W :E, :S :N)
    270 (case facing :E :S, :N :E, :W :N, :S :W)))

(defn decode [inst]
  [(keyword (subs inst 0 1))
   (read-string (subs inst 1))])

(defn part1 []
  (-> (reduce (fn [nav head]
                (let [[dir size] (decode head)]
                  (case dir
                    (:L :R) (update nav :facing #(turn->dir dir size %))
                    (update nav :ship #(matrix/add % (matrix/scale (dir->delta dir (dir->delta (nav :facing))) size))))))
             {:ship [0 0] :facing :E}
             input)
      :ship
      (compute-distance [0 0]))
  )

(defn rotate-by [turn angle [x y]]
  (case (if (= turn :L) angle (- 360 angle))
    90 [(* -1 y) x]
    180 [(* -1 x) (* -1 y)]
    270 [y (* -1 x)]))

(defn part2 []
  (-> (reduce (fn [nav head]
                (let [[dir size] (decode head)]
                  (case dir
                    (:L :R) (update nav :waypoint #(rotate-by dir size %))
                    :F (update nav :ship #(matrix/add % (matrix/scale (nav :waypoint) size)))
                    (update nav :waypoint #(matrix/add % (matrix/scale (dir->delta dir) size))))))
              {:ship [0 0] :waypoint [-1 10]}
              input)
      :ship
      (compute-distance [0 0])))

;; part 1:  845
;; part 2:  27016

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
