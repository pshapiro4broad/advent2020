(ns day12
  (:require [clojure.string :as str]))

(def input
  (-> "src/day12-input.txt"
      slurp
      str/split-lines))

(def test-input '("F10" "N3" "F7" "R90" "F11"))

(defn compute-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x1 x2))
     (Math/abs ^int (- y1 y2))))

(def dir->delta
  {:N [-1 0]
   :S [1 0]
   :E [0 1]
   :W [0 -1]})

(defn turn->dir [turn size facing]
  (case turn
    :L (case size
         90 (case facing :E :N, :N :W, :W :S, :S :E)
         180 (case facing :E :W, :N :S, :W :E, :S :N)
         270 (case facing :E :S, :N :E, :W :N, :S :W))
    :R (case size
         90 (case facing :E :S, :N :E, :W :N, :S :W)
         180 (case facing :E :W, :N :S, :W :E, :S :N)
         270 (case facing :E :N, :N :W, :W :S, :S :E))
    nil))

(defn decode [inst]
  [(keyword (subs inst 0 1))
   (read-string (subs inst 1))])

(defn part1 []
  (loop [input input
         facing :E
         pos [0 0]]
    (if (empty? input)
      (compute-distance [0 0] pos)
      (let [inst (decode (first input))
            dir (first inst)
            size (last inst)
            turn (turn->dir dir size facing)
            ]
        (if turn
          (recur (next input) turn pos)
          (recur (next input) facing (mapv + pos (mapv #(* % size) (dir->delta (if (dir->delta dir) dir facing)))))
          )
        )
      )
    )
  )

(defn rotate-by [turn size [x y]]
  (case turn
    :L (case size
         90 [(* -1 y) x]
         180 [(* -1 x) (* -1 y)]
         270 [y (* -1 x)])
    :R (case size
         90 [y (* -1 x)]
         180 [(* -1 x) (* -1 y)]
         270 [(* -1 y) x])))

(defn part2 []
  (loop [input input
         ship [0 0]
         waypoint [-1 10]]
    (if (empty? input)
      (compute-distance [0 0] ship)
      (let [inst (decode (first input))
            dir (first inst)
            size (last inst)
            ]
        (case dir
          (:L :R) (recur (next input) ship (rotate-by dir size waypoint))
          :F (recur (next input) (map + ship (mapv #(* % size) waypoint)) waypoint)
          (recur (next input) ship (map + waypoint (mapv #(* % size) (dir->delta dir))))
          )
        )
      )
    )
  )

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
)
