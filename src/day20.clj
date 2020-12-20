(ns day20
  (:require [clojure.string :as str]))

(def input
  (-> "src/day20-input.txt"
      slurp
      (str/split #"\n\n")))

; tiles are 10x10
(def tile-size 10)
(def tile-edges
  (let [edges [(for [x (range tile-size)] [x 0])
               (for [y (range tile-size)] [0 y])
               (for [y (range tile-size)] [(dec tile-size) y])
               (for [x (range tile-size)] [x (dec tile-size)])]]
    (into edges (map reverse edges))))

(defn edge-id [edge]
  (read-string (str "2r" (apply str (replace {\. \0 \# \1} edge)))))

(defn edges [tile]
  (map edge-id
       (for [edge tile-edges]
         (for [[x y] edge]
           (nth (nth tile y) x)))))

(defn all-edges [parsed skip]
  (into #{} (flatten (map #(and (not= skip %) (% :edges)) parsed))))

(defn parse-tile [text]
  (let [lines (str/split-lines text)
        tile-id (->> (first lines) (re-find #"Tile (\d+):") fnext read-string)
        tile (rest lines)]
    {:id tile-id
     :edges (edges tile)}))

(defn part1 []
  (let [parsed (map parse-tile input)
        ]
    (->> parsed
         (filter #(= 4 (count (filter (all-edges parsed %) (% :edges)))))
         (map :id)
         (reduce *))))

(defn part2 []
  )

;(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  ;)