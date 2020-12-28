(ns day15
  (:require [clojure.string :as str]))

(def input
  (-> "11,0,1,10,5,19"
      (str/split #",")
      (->> (map read-string))))

(defn compute-play [state prev]
  (let [s (get state prev)]
    (if (= (count s) 1)
     0
     (- (last s) (first s)))))

(defn play-game [input turns]
  (loop [turn (inc (count input))
         state (zipmap input (map vector (range 1 turn)))
         prev (last input)]
    (let [play (compute-play state prev)]
      (if (= turn turns)
        play
        (recur (inc turn) (update state play #(if % [(last %) turn] [turn])) play)))))

(defn part1 []
  (play-game input 2020))

(defn part2 []
  (play-game input 30000000))

; part 1:  870
; part 2:  9136

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
)