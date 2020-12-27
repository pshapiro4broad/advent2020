(ns day23
  (:require [clojure.string :as str]))

(def input "364297581")

(defn find-destination [cup skip]
  (loop [cup (if (= cup 1) 9 (dec cup))]
    (if (skip cup)
      (recur (if (= cup 1) 9 (dec cup)))
      cup)))

(defn cups-next [cups cup]
  (nth cups cup))

(defn make-cups [input total]
  (let [data (->> (str/split input #"")
                  (map read-string)
                  (#(concat % (range (inc (count input)) (inc total)))))]
    (loop [cups (into [] (repeat (count data) 0))
           index 0]
      (if (= index (dec total))
        {:cups (assoc cups (last data) (first data)) :current (first data)}
        (recur (assoc cups (nth data index) (nth data (inc index)))
               (inc index))))))

(defn print-cups [{cups :cups current :current}]
  (loop [next-cup (nth cups current)
         result [current]]
    (if (= next-cup current)
      result
      (recur (nth cups next-cup) (conj result next-cup)))))

(def test-input "389125467")

(defn move [{cups :cups current :current}]
  (let [c1 (cups-next cups current)
        c2 (cups-next cups c1)
        c3 (cups-next cups c2)
        cups (assoc cups current (cups-next cups c3))
        destination (find-destination current #{c1 c2 c3})
        old-next (cups-next cups destination)
        cups (assoc cups destination c1)
        cups (assoc cups c3 old-next)]
    {:cups cups :current (cups-next cups current)}))

(defn part1 []
  (let [cups (make-cups input (count input))]
    (loop [times 100
           cups cups]
      (if (zero? times)
        (->> (print-cups {:cups (cups :cups) :current 1})
            next
            (apply str))
        (recur (dec times) (move cups))))))

(defn part2 [num-cups num-moves]
  (let [cups (make-cups input num-cups)]
    (loop [times num-moves
           cups cups]
      (if (zero? times)
        (->> (print-cups {:cups (cups :cups) :current 1})
             next
             (take 2)
             (reduce *))
        (recur (dec times) (move cups))))))

;(assert (not= (part1) 47382659) "broken")
;(comment
  (println "part 1: " (part1))
  ;(println "part 2: " (part2))
  ;)