(ns day5
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day5-input.txt"
      slurp
      str/split-lines))

(defn to-id [seat]
  (letfn [(bin-val [s] (edn/read-string (str "2r" s)))]
    (-> seat
        (str/replace #"F|B|R|L" {"F" "0" "B" "1" "L" "0" "R" "1"})
        (#(+ (* 8 (bin-val (subs % 0 7)))
             (bin-val (subs % 7 10)))))))

(defn part1 []
  (apply max (map to-id input)))

(defn part2 []
  (let [ids (map to-id input)
        front (apply min ids)
        back (apply max ids)]
    (->> (set ids)
         (set/difference (set (range front back)))
         first)))

;(println "part 1: " (part1))
;(println "part 2: " (part2))
