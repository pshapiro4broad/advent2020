(ns day22
  (:require [clojure.string :as str]))

(def input
  (-> "src/day22-input.txt"
      slurp
      (str/split #"\n\n")))

(defn make-queue [s]
  (into (clojure.lang.PersistentQueue/EMPTY) s))

(defn make-player [text]
  (->> text
       str/split-lines
       (drop 1)
       (map read-string)
       make-queue))

(defn play-game [p1 p2]
  (loop [p1 p1
         p2 p2]
    (cond
      (empty? p1) p2
      (empty? p2) p1
      :else (let [c1 (peek p1)
                  c2 (peek p2)]
              (if (> c1 c2)
                (recur (-> p1 pop (conj c1) (conj c2)) (pop p2))
                (recur (pop p1) (-> p2 pop (conj c2) (conj c1))))))))

(defn compute-score [player]
  (loop [player player
         score 0
         index (count player)]
    (if (empty? player)
      score
      (recur (pop player) (+ score (* index (peek player))) (dec index)))))

(defn part1 []
  (let [player1 (make-player (first input))
        player2 (make-player (second input))]
    (-> (play-game player1 player2)
        compute-score)))


(defn play-game2 [p1 p2]
  (loop [p1 p1
         p2 p2
         seen-hands #{}]
    (cond
      (empty? p1) [p2 :p2]
      (or (empty? p2) (seen-hands p1) (seen-hands p2)) [p1 :p1]
      :else (let [c1 (peek p1)
                  c2 (peek p2)
                  seen-hands (-> seen-hands (conj p1) (conj p2))
                  p1 (pop p1)
                  p2 (pop p2)]
              (cond
                (and (>= (count p1) c1) (>= (count p2) c2))
                (if (or (> (reduce max (take c1 p1)) (reduce max (take c2 p2)))
                     (= :p1 (second (play-game2 (->> p1 (take c1) make-queue)
                                                (->> p2 (take c2) make-queue)))))
                  (recur (-> p1 (conj c1) (conj c2)) p2 seen-hands)
                  (recur p1 (-> p2 (conj c2) (conj c1)) seen-hands))
                (> c1 c2)
                (recur (-> p1 (conj c1) (conj c2)) p2 seen-hands)
                :else
                (recur p1 (-> p2 (conj c2) (conj c1)) seen-hands))))))

(defn part2 []
  (let [player1 (make-player (first input))
        player2 (make-player (second input))]
    (-> (play-game2 player1 player2)
        first
        compute-score)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )