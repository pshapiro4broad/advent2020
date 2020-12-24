(ns day24
  (:require [clojure.string :as str]))

(def input
  (-> "src/day24-input.txt"
      slurp
      str/split-lines))

; e, se, sw, w, nw, and ne
(defn parse-line [line]
  (map keyword (re-seq #"e|se|sw|w|nw|ne" line)))

(def dir->delta
  {:e [1 0], :se [0 1], :sw [-1 1], :w [-1 0], :nw [0 -1], :ne [1 -1]})

(defn flip-tile [tiles tile]
  (if (get tiles tile)
    (disj tiles tile)
    (conj tiles tile)))

(defn load-tiles [input]
  (reduce
    (fn [tiles line]
      (flip-tile
        tiles
        (reduce (fn [pos delta] (mapv + delta pos))
                [0 0]
                (map dir->delta line))))
    #{}
    (->> input (map parse-line))))

(defn part1 []
  (count (load-tiles input)))

;; copied from day 17, more or less

(defn neighborhood [[x y]]
  (for [[dx dy] (vals dir->delta)]
    [(+ x dx) (+ y dy)]))

(defn next-cycle [tiles]
  (set (for [[tile count] (frequencies (mapcat neighborhood tiles))
             :when (or (= count 2)
                       (and (contains? tiles tile)
                            (or (= count 1) (= count 2))))]
         tile)))

(defn part2 []
  (loop [cycles 100
         tiles (load-tiles input)]
    (if (zero? cycles)
      (count tiles)
      (recur (dec cycles) (next-cycle tiles)))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )