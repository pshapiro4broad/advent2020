(ns day10
  (:require [clojure.string :as str]))

(def input
  (-> "src/day10-input.txt"
      slurp
      str/split-lines
      (->> (map read-string))))

(def test-input '(16 10 15 5 1 11 7 19 6 12 4))

(def test-input-2 '(28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))

(defn part1 []
  (let [c (vec (sort (conj input 0 (+ 3 (reduce max input)))))]
    (->> (range (dec (count c)))
         (map #(- (nth c (inc %)) (nth c %)))
         frequencies
         (#(* (% 1) (% 3))))))

(defn part2 []
  (let [c (vec (sort (conj input 0 (+ 3 (reduce max input)))))]
    (loop [sums {}
           index 0]
      (if (= index (count c))
        (sums (last c))
        (recur
          (assoc sums
            (nth c index)
            (->> (range 1 4)
                 (map #(sums (- (nth c index) %) 0))
                 (reduce +)
                 (max 1)))
          (inc index))))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2)))