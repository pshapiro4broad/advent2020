(ns day23
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def input "364297581")

(defn find-destination [cup skip size]
  (loop [cup (if (= cup 1) (dec size) (dec cup))]
    (if (skip cup)
      (recur (if (= cup 1) (dec size) (dec cup)))
      cup)))

(defn make-cups [input total]
  (let [data (->> (str/split input #"")
                  (map read-string)
                  (#(concat % (range (inc (count input)) (inc total)))))
        head (first data)
        cups (int-array (inc (count data)))]
    (loop [data data]
      (if (= 1 (count data))
        (do
          (aset-int cups (first data) head)
          {:cups ^ints cups :current head})
        (do
          (aset-int cups (first data) (fnext data))
          (recur (next data)))))))

(defn print-cups [{^ints cups :cups current :current}]
  (loop [next-cup (aget cups current)
         result [current]]
    (if (= next-cup current)
      result
      (recur (aget cups next-cup) (conj result next-cup)))))

(defn move [{^ints cups :cups current :current}]
  (let [c1 (aget cups current)
        c2 (aget cups c1)
        c3 (aget cups c2)
        destination (find-destination current #{c1 c2 c3} (count cups))
        old-next (aget cups destination)]
    (aset-int cups current (aget cups c3))
    (aset-int cups destination c1)
    (aset-int cups c3 old-next)
    {:cups cups :current (aget cups current)}))

(defn part1 []
  (let [cups (make-cups input (count input))]
    (loop [times 100
           cups cups]
      (if (zero? times)
        (->> (print-cups {:cups (cups :cups) :current 1})
            next
            (apply str))
        (recur (dec times) (move cups))))))

(def num-moves 10000000)
(def num-cups 1000000)

(defn part2 []
  (let [cups (make-cups input num-cups)]
    (loop [times num-moves
           cups cups]
      (if (zero? times)
        (->> (print-cups {:cups (cups :cups) :current 1})
             next
             (take 2)
             (reduce *))
        (recur (dec times) (move cups))))))

;; part 1:  47382659
;; part 2:  42271866720

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )