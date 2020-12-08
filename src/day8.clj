(ns day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day8-input.txt"
      slurp
      str/split-lines))

(def test-input
  '("nop +0"
     "acc +1"
     "jmp +4"
     "acc +3"
     "jmp -3"
     "acc -99"
     "acc +1"
     "jmp -4"
     "acc +6"
     ))

(defn parse-inst [line]
  (let [split (str/split line #" ")]
    [(keyword (first split)) (edn/read-string (last split))]))

(defn make-program [input]
  (vec (map parse-inst input)))

(defn run-program [program]
  (loop [acc 0
         pc 0
         seen-pcs #{}]
    (cond
      (seen-pcs pc) [:inf acc]
      (= pc (count program)) [:end acc]
      :else
      (let [inst (nth program pc)]
        (case (first inst)
          :nop (recur acc (inc pc) (conj seen-pcs pc))
          :acc (recur (+ acc (last inst)) (inc pc) (conj seen-pcs pc))
          :jmp (recur acc (+ pc (last inst)) (conj seen-pcs pc)))))))

(defn part1 []
  (last (run-program (make-program input))))

(defn part2 []
  (let [program (make-program input)]
    (->> (range (count program))
         (filter (fn [pc] (#{:nop :jmp} (first (nth program pc)))))
         (map (fn [pc]
                (let [inst (nth program pc)]
                  (assoc program pc [(if (= :nop (first inst)) :jmp :nop) (last inst)]))))
         (map run-program)
         (filter (fn [[op _val]] (= op :end)))
         first
         last)))

(println "part 1: " (part1))
(println "part 2: " (part2))
