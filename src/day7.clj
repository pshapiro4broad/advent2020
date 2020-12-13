(ns day7
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day7-input.txt"
      slurp
      str/split-lines))

(def test-input
  '("light red bags contain 1 bright white bag, 2 muted yellow bags."
     "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
     "bright white bags contain 1 shiny gold bag."
     "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
     "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
     "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
     "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
     "faded blue bags contain no other bags."
     "dotted black bags contain no other bags."))

(def test-input2
  '("shiny gold bags contain 2 dark red bags."
     "dark red bags contain 2 dark orange bags."
     "dark orange bags contain 2 dark yellow bags."
     "dark yellow bags contain 2 dark green bags."
     "dark green bags contain 2 dark blue bags."
     "dark blue bags contain 2 dark violet bags."
     "dark violet bags contain no other bags."))

; "faded blue bags contain no other bags."
; "dull aqua bags contain 4 dark fuchsia bags, 1 shiny purple bag."
; => {"dull-aqua" {"dark-fuchsia" 4, "shiny-purple" 1}}
(defn parse-bag
  "from entry, create a bag. a bag is map of a bag name to a map of inner bag names to the inner bag count"
  [entry]
  (let [outer (str/split entry #"contain")
        inner (str/split (nth outer 1) #",")]
    {(str/join "-" (take 2 (str/split (nth outer 0) #" ")))
     (->> inner
          (map #(str/split % #" "))
          (map #(if (= (nth % 1) "no")
                  {}
                  {(str/join "-" (subvec % 2 4))
                   (edn/read-string (nth % 1))}))
          (reduce merge))}))

(defn bag-contains? [bags bag-type bag]
  ((bags bag) bag-type))

(defn deep-bag-contains? [bags bag-type bag]
  (or (bag-contains? bags bag-type bag)
      (->> (keys (bags bag))
           (map #(deep-bag-contains? bags bag-type %))
           (filter some?)
           first)))

(defn part1 []
  (let [bags (->> input
                  (map parse-bag)
                  (reduce merge))]
    (->> (keys bags)
         (filter #(deep-bag-contains? bags "shiny-gold" %))
         count)))

(defn deep-bag-count [bags bag]
  (->> (bags bag)
       (map (fn [[k v]] (+ v (* v (deep-bag-count bags k)))))
       (reduce +)))

(defn part2 []
  (let [bags (->> input
                  (map parse-bag)
                  (reduce merge))]
    (deep-bag-count bags "shiny-gold")))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )