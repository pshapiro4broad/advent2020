(ns main)
(require 'day1
         'day2
         'day3
         'day4
         'day5
         'day6
         'day7
         'day8
         'day9
         'day10
         'day11)

(defn run []
  (->> (range 1 10)
       (map #(do
               (println "day" %)
               (println "part 1:" ((eval (symbol (str "day" % "/part1")))))
               (println "part 2:" ((eval (symbol (str "day" % "/part2")))))))))