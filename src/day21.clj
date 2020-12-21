(ns day21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day21-input.txt"
      slurp
      str/split-lines))

;mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
(defn parse-food [line]
  (let [[_ ingredients allergens] (re-find #"(.*) \(contains (.*)\)" line)]
    {:ing (str/split ingredients #" ")
     :alg (str/split allergens #", ")}))

(defn collect-allergens [foods]
  (reduce
    (fn [allergens {alg :alg ing :ing}]
      (let [ing-set (set ing)]
        (reduce (fn [allergens allergen]
                  (update allergens allergen
                          (fn [v] (if v (set/intersection v ing-set) ing-set))))
                allergens
                alg)))
    {}
    foods))

(defn part1 []
  (let [foods (map parse-food input)
        allergens (->> (collect-allergens foods)
                       vals
                       (reduce set/union))]
    (->> foods
         (map :ing)
         flatten
         (remove #(contains? allergens %))
         frequencies
         vals
         (reduce +))))

(defn remove-all [algs iso]
  (->> algs
       (map (fn [[k v]] {k (disj v (first (last iso)))}))
       (into {})))

(defn isolate-allergens [allergens]
  (loop [allergens allergens
         isolated {}]
    (if (= (count isolated) (count allergens))
      isolated
      (let [new-isolate (first (filter (fn [[_ v]] (= 1 (count v))) allergens))]
       (recur (remove-all allergens new-isolate) (conj isolated new-isolate))))))

(defn part2 []
  (->> (map parse-food input)
       collect-allergens
       isolate-allergens
       (into (sorted-map))
       vals
       (map first)
       (str/join ",")))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )