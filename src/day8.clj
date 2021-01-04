(ns day8
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day8-input.txt"
      slurp
      str/split-lines))

(defn parse-inst [line]
  (let [split (str/split line #" ")]
    [(keyword (first split)) (edn/read-string (last split))]))

(def get-op first)
(def get-acc last)

(defn make-program [input]
  (vec (map parse-inst input)))

(defn run-program [program]
  (loop [acc 0
         pc 0
         seen-pcs #{}]
    (cond
      (contains? seen-pcs pc) [:inf acc]
      (= pc (count program)) [:end acc]
      :else
      (let [[op arg] (nth program pc)]
        (recur
          (if (= op :acc) (+ acc arg) acc)
          (if (= op :jmp) (+ pc arg) (inc pc))
          (conj seen-pcs pc))))))

(defn part1 []
  (get-acc (run-program (make-program input))))

(defn part2 []
  (let [program (make-program input)]
    (->> (range (count program))
         (filter (fn [pc] (#{:nop :jmp} (get-op (nth program pc)))))
         (map (fn [pc]
                (let [[op arg] (nth program pc)]
                  (assoc program pc [(if (= :nop op) :jmp :nop) arg]))))
         (map run-program)
         (filter (fn [[op _]] (= op :end)))
         first
         get-acc)))

; part 1:  1489
; part 2:  1539

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )