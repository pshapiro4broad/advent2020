(ns day10
  (:require [clojure.string :as str]))

(def input
  (-> "src/day10-input.txt"
      slurp
      str/split-lines
      (->> (map read-string))))

(defn part1 []
  (let [c (sort (conj input 0 (+ 3 (reduce max input))))]
    (->> (range (dec (count c)))
         (map #(- (nth c (inc %)) (nth c %)))
         frequencies
         (#(* (% 1) (% 3))))))

(defn part2 []
  (let [c (sort (conj input 0 (+ 3 (reduce max input))))]
    (loop [sums {}
           index 0]
      (if (= index (count c))
        (sums (last c))
        (recur
          (assoc sums
            (nth c index)
            (->> (range 1 4)
                 (map #(get sums (- (nth c index) %) 0))
                 (reduce +)
                 (max 1)))
          (inc index))))))

; part 1:  1980
; part 2:  4628074479616

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )