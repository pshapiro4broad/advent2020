(ns day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day6-input.txt"
      slurp
      (str/split #"\n\n")))

(defn part1 []
  (->> input
       (map #(str/replace % #"\n" ""))
       (map seq)
       (map set)
       (map count)
       (reduce +)))

(defn group-total [entry]
  (->> entry
       str/split-lines
       (map seq)
       (map set)
       (reduce set/intersection)
       count))

(defn part2 []
  (->> input
       (map group-total)
       (reduce +)))

;(println "part 1: " (part1))
;(println "part 2: " (part2))
