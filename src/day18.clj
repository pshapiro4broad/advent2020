(ns day18
  (:require [clojure.string :as str]))


(def input
  (->> "src/day18-input.txt"
       slurp
       str/split-lines))

(defn reduce-exp [exp]
  (let [op (re-find #"(\d+) ([\+|\*]) (\d+)" exp)
        op-loc (and op (str/index-of exp (first op)))
        paren (re-find #"\(([^\(\)]+)\)" exp)
        paren-loc (and paren (str/index-of exp (first paren)))]
    (cond
      paren
      (reduce-exp (str (subs exp 0 paren-loc) (reduce-exp (nth paren 1))
                       (subs exp (+ paren-loc (count (first paren))))))
      op
      (reduce-exp (str (subs exp 0 op-loc) ((if (= "+" (nth op 2)) + *)
                                            (read-string (nth op 1))
                                            (read-string (nth op 3)))
                       (subs exp (+ op-loc (count (first op))))))
      :else (read-string exp))))

(defn part1 []
  (->> input
       (map reduce-exp)
       (reduce +)))

(defn reduce-exp2 [exp]
  (let [plus (re-find #"(\d+) \+ (\d+)" exp)
        plus-loc (and plus (str/index-of exp (first plus)))
        mult (re-find #"(\d+) \* (\d+)" exp)
        mult-loc (and mult (str/index-of exp (first mult)))
        paren (re-find #"\(([^\(\)]+)\)" exp)
        paren-loc (and paren (str/index-of exp (first paren)))]
    (cond
      paren
      (reduce-exp2 (str (subs exp 0 paren-loc) (reduce-exp2 (nth paren 1))
                       (subs exp (+ paren-loc (count (first paren))))))
      plus
      (reduce-exp2 (str (subs exp 0 plus-loc) (+ (read-string (nth plus 1))
                                                (read-string (nth plus 2)))
                       (subs exp (+ plus-loc (count (first plus))))))
      mult
      (reduce-exp2 (str (subs exp 0 mult-loc) (* (read-string (nth mult 1))
                                                (read-string (nth mult 2)))
                       (subs exp (+ mult-loc (count (first mult))))))
      :else (read-string exp))))

(defn part2 []
  (->> input
       (map reduce-exp2)
       (reduce +)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )