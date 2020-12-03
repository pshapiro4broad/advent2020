(ns day2
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (-> "src/day2-input.txt"
      slurp
      str/split-lines))

(defn count-chars-in-string [c s]
  (->> (seq s)
      (filter #(= % c))
      count))

;;"2-6 c: fcpwjqhcgtffzlbj"

(defn parse-entry [entry]
  (let* [found (re-find #"(\d+)-(\d+) (\w): (\w+)" entry)
         first-num (edn/read-string (nth found 1))
         second-num (edn/read-string (nth found 2))
         c (first (nth found 3))
         password (nth found 4)]
    (list first-num second-num c password)))

(defn count-all [pred]
  (->> input
       (map parse-entry)
       (filter #(apply pred %))
       count))

(defn is-valid1 [min max c password]
  (let [num-chars (count-chars-in-string c password)]
    (and (>= num-chars min) (<= num-chars max))))

(defn part1 []
  (count-all is-valid1))

(defn is-valid2 [first second c password]
  (not= (= c (nth password (dec first)))
        (= c (nth password (dec second)))))

(defn part2 []
  (count-all is-valid2))

(println "part 1: " (part1))
(println "part 2: " (part2))
