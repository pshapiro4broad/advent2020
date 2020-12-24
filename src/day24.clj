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
(def adjacent (vals dir->delta))

(defn possible-active [tiles]
  (-> (for [tile tiles
            near adjacent]
        (mapv + tile near))
      distinct))

(defn active-neighbors [all-alive tile]
  (count
    (for [delta adjacent
          :let [neighbor (mapv + tile delta)]
          :when (get all-alive neighbor)]
      neighbor)))

(defn next-active [tiles tile]
  (let [active? (get tiles tile)
        active-neighbors (active-neighbors tiles tile)]
    (if active?
      (or (= active-neighbors 1) (= active-neighbors 2))
      (= active-neighbors 2))))

(defn next-cycle [tiles]
  (->> (possible-active tiles)
       (filter #(next-active tiles %))
       (into #{})))

;; works, but is slow
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