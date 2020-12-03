(ns day1
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.reducers :as r]))

(def input
  (map edn/read-string
       (-> "src/day1-input.txt"
           slurp
           str/split-lines)))

(defn first-2020 [size]
  (first (filter (fn [tuple] (= 2020 (r/fold + tuple))) (combo/combinations input size))))

(defn part1 []
  (r/fold * (first-2020 2)))

(defn part2 []
  (r/fold * (first-2020 3)))

(println "part 1: " (part1))
(println "part 2: " (part2))
