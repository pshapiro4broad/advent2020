(ns day1
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def input
  (->> "src/day1-input.txt"
       slurp
       str/split-lines
       (map read-string)))

(defn first-2020 [size]
  (->> (combo/combinations input size)
       (filter #(= 2020 (reduce + %)))
       first))

(defn part1 []
  (reduce * (first-2020 2)))

(defn part2 []
  (reduce * (first-2020 3)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )