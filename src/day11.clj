(ns day11
  (:require [clojure.string :as str]))

(def input
  (-> "src/day11-input.txt"
      slurp
      str/split-lines))

(def test-input
  (->> "src/day11-test-input.txt"
       slurp
       str/split-lines))

(def seat \L)
(def occupied \#)
(def vacant \.)

(defn get-seat [seats row col]
  (nth (nth seats row nil) col nil))

(defn occupied? [seats [row col]]
  (= occupied (get-seat seats row col)))

(defn num-rows [seats] (count seats))
(defn num-cols [seats] (count (first seats)))

(def adjacent
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not= 0 x y)]
    [x y]))

(defn count-occupied1 [seats row col]
  (->> adjacent
       (map #(vector (+ row (first %)) (+ col (last %))))
       (filter #(occupied? seats %))
       count))

(defn seat-next [seats row col count-occupied max-occupied]
  (let [count (count-occupied seats row col)
        current (get-seat seats row col)]
    (cond
      (and (= current seat) (zero? count)) occupied
      (and (= current occupied) (>= count max-occupied)) seat
      :else current)))

(defn next-round [seats count-occupied max-occupied]
  (->> (for [row (range (num-rows seats))
             col (range (num-cols seats))]
         (seat-next seats row col count-occupied max-occupied))
       (partition (count (first seats)))
       ;; for speed, make the next round a vector of vectors
       (map vec)
       vec))

(defn find-steady-state [seats count-occupied max-occupied]
  (loop [seats seats
         count 0]
    (let [new-seats (next-round seats count-occupied max-occupied)]
      (if (= new-seats seats)
        (-> seats
            flatten
            frequencies
            (get occupied))
        (recur new-seats (inc count))))))

(defn part1 []
  (find-steady-state input count-occupied1 4))

(defn find-occupied [seats row col [delta-row delta-col]]
  (loop [row (+ delta-row row)
         col (+ delta-col col)]
    (if (or (= row -1) (= col -1)
            (= row (num-rows seats)) (= col (num-cols seats))
            (not= (get-seat seats row col) vacant))
      (= (get-seat seats row col) occupied)
      (recur (+ delta-row row) (+ delta-col col)))))

(defn count-occupied2 [seats row col]
  (->> adjacent
       (map #(find-occupied seats row col %))
       (filter true?)
       count))

(defn part2 []
  (find-steady-state input count-occupied2 5))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2)))