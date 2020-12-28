(ns day7
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def input
  (-> "src/day7-input.txt"
      slurp
      str/split-lines))

; "faded blue bags contain no other bags."
; "dull aqua bags contain 4 dark fuchsia bags, 1 shiny purple bag."
; => {"dull-aqua" {"dark-fuchsia" 4, "shiny-purple" 1}}
(defn parse-bag
  "from entry, create a bag. a bag is map of a bag name to a map of inner bag names to the inner bag count"
  [entry]
  (let [[bag inside] (str/split entry #"contain")
        inner (str/split inside #",")]
    {(str/join "-" (take 2 (str/split bag #" ")))
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

; part 1:  192
; part 2:  12128

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )