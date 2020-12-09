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

(def get-op first)
(def get-arg last)

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
      (let [inst (nth program pc)
            op (get-op inst)
            arg (get-arg inst)]
        (recur
          (if (= op :acc) (+ acc arg) acc)
          (if (= op :jmp) (+ pc arg) (inc pc))
          (conj seen-pcs pc))))))

(defn part1 []
  (last (run-program (make-program input))))

(defn part2 []
  (let [program (make-program input)]
    (->> (range (count program))
         (filter (fn [pc] (#{:nop :jmp} (get-op (nth program pc)))))
         (map (fn [pc]
                (let [inst (nth program pc)]
                  (assoc program pc [(if (= :nop (get-op inst)) :jmp :nop) (get-arg inst)]))))
         (map run-program)
         (filter (fn [[op _arg]] (= op :end)))
         first
         last)))

;(println "part 1: " (part1))
;(println "part 2: " (part2))
