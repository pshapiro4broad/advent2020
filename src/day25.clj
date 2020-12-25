(ns day25)

(def input [14788856 19316454])

(defn next-value [value subject]
  (rem (* value subject) 20201227))

(defn transform [subject cycle]
  (loop [value 1
         cycle cycle]
    (if (zero? cycle)
      value
      (recur (next-value value subject) (dec cycle)))))

(defn transform-to [subject target]
  (loop [value 1
         cycle 0]
    (if (= value target)
      cycle
      (recur (next-value value subject) (inc cycle)))))

(def initial-subject 7)

(defn part1 []
  (transform (last input) (transform-to initial-subject (first input))))

(defn part2 []
  )

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )