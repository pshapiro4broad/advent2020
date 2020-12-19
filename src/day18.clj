(ns day18
  (:require [clojure.string :as str]))

(def input
  (->> "src/day18-input.txt"
       slurp
       str/split-lines))

(defn reduce-exp [exp]
  (let [op (re-find #"(\d+) ([\+|\*]) (\d+)" exp)
        paren (re-find #"\(([^\(\)]+)\)" exp)
        make-reduced
        (fn [found reduced]
          (reduce-exp (str/replace-first exp (first found) (str reduced))))]
    (cond
      paren (make-reduced paren (reduce-exp (nth paren 1)))
      op (make-reduced op ((if (= "+" (nth op 2)) + *)
                           (read-string (nth op 1))
                           (read-string (nth op 3))))
      :else (read-string exp))))

(defn part1 []
  (->> input
       (map reduce-exp)
       (reduce +)))

(defn reduce-exp2 [exp]
  (let [plus (re-find #"(\d+) \+ (\d+)" exp)
        mult (re-find #"(\d+) \* (\d+)" exp)
        paren (re-find #"\(([^\(\)]+)\)" exp)
        make-reduced
        (fn [found reduced]
          (reduce-exp2 (str/replace-first exp (first found) (str reduced))))]
    (cond
      paren (make-reduced paren (reduce-exp2 (nth paren 1)))
      plus (make-reduced plus (+ (read-string (nth plus 1))
                                 (read-string (nth plus 2))))
      mult (make-reduced mult (* (read-string (nth mult 1))
                                 (read-string (nth mult 2))))
      :else (read-string exp))))

(defn part2 []
  (->> input
       (map reduce-exp2)
       (reduce +)))

;(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  ;)