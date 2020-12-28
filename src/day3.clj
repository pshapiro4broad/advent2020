(ns day3
  (:require [clojure.string :as str]))

(def input
  (-> "src/day3-input.txt"
      slurp
      str/split-lines))

(def num-rows (count input))
(def num-cols (count (first input)))

(defn tree-at? [row col]
  (= \# (nth (nth input row nil) (mod col num-cols))))

(defn count-trees [[right down]]
  (->> (range num-rows)
       (filter #(tree-at? (* % down) (* % right)))
       count))

(defn part1 []
  (count-trees [3 1]))

(defn part2 []
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map count-trees)
       (reduce *)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )