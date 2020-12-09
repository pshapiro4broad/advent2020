(ns day9
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day9-input.txt"
      slurp
      str/split-lines
      (->> (map edn/read-string))))

(def test-input
  '(35 20 15 25 47 40 62 55 65 95 102 117 150 182 127
     219 299 277 309 576))

(defn valid [vec value]
  (->> (combo/combinations vec 2)
       (filter #(= value (reduce + %)))
       first))

(defn part1 []
  (let [preamble 25
        data (vec input)]
    (->>
      (range preamble (count data))
      (filter #(not (valid (subvec data (- % preamble) %) (nth data %))))
      first
      (nth data))))

(defn part2 []
  (let [invalid (part1)
        data (vec input)]
    (->>
      (for [length (range 2 (count data))
            index (range 0 (- (count data) length))]
        (subvec data index (+ index length)))
      (filter #(= invalid (reduce + %)))
      first
      (#(+ (reduce min %) (reduce max %))))))

;(println "part 1: " (part1))
;(println "part 2: " (part2))
