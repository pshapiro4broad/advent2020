(ns day16
  (:require [clojure.string :as str]))

; arrival platform: 45-666 or 678-952
(defn parse-rule [text]
  (let [parts (re-find #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" text)
        low1 (read-string (nth parts 2))
        high1 (read-string (nth parts 3))
        low2 (read-string (nth parts 4))
        high2 (read-string (nth parts 5))]
    {:name (nth parts 1)
     :rule (fn [x] (or (and (>= x low1) (<= x high1))
                       (and (>= x low2) (<= x high2))))}))

(def test-lines "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")
(def test-lines2 "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")

(def input
  (let [lines (->
                ;test-lines2
                "src/day16-input.txt"
                  slurp
                  (str/split #"\n\n"))]
    {:rules (->> (first lines)
                 str/split-lines
                 (map parse-rule))
     :your (-> (second lines)
               str/split-lines
               fnext
               (str/split #",")
               (->> (map read-string)))
     :nearby (->> (last lines)
                  str/split-lines
                  (drop 1)
                  (map #(str/split % #","))
                  (map #(map read-string %)))}))

(defn part1 []
  (->> (input :nearby)
       flatten
       (filter #(not (->> (input :rules)
                          (map :rule)
                          (some (fn [rule] (rule %))))))
       (reduce +)))

(defn part2 []
  (let [valid-nearby (->> (input :nearby)
                          (filter
                            #(->> % (every?
                                      (fn [val] (->> (input :rules)
                                                     (map :rule)
                                                     (some (fn [rule] (rule val)))))))))
        ranked (->> valid-nearby
                    (apply (partial map vector))
                    (map (fn [vals]
                           (->> (input :rules)
                                (filter #(every? (% :rule) vals))
                                (map :name))))
                    (zipmap (range (count (input :rules))))
                    (sort-by #(count (last %))))
        rule-map (loop [ranked ranked
                        rule-map {}]
                   (if ranked
                     (let [name (->> (fnext (first ranked))
                                     (filter #(not (some (fn [v] (= v %)) (vals rule-map))))
                                     first)]
                       (recur (next ranked) (assoc rule-map (ffirst ranked) name)))
                     rule-map))]
    (->> rule-map
         (filter (fn [[_k v]] (= (str/index-of v "departure") 0)))
         (map first)
         (map #(nth (input :your) %))
         (reduce *))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )