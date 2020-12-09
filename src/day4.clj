(ns day4
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))
(def input
  (-> "src/day4-input.txt"
      slurp
      (str/split #"\n\n")))

(defn parse-id [text]
  (->> (str/split text #"[ \n]")
       (map #(str/split % #":"))
       flatten
       (apply hash-map)
       walk/keywordize-keys))

;byr (Birth Year)
;iyr (Issue Year)
;eyr (Expiration Year)
;hgt (Height)
;hcl (Hair Color)
;ecl (Eye Color)
;pid (Passport ID)
;cid (Country ID)

(def required-keys
  '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defn valid-id1 [id]
  (= (count required-keys)
     (count (select-keys id required-keys))))

(defn part1 []
  (->> input
       (map parse-id)
       (filter valid-id1)
       count))

(defn valid-yr [yr low high]
  (let [y (edn/read-string (re-find #"^\d{4}$" yr))]
    (and y (>= y low) (<= y high))))

;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
(defn valid-hgt [v]
  (let [hgt-cm (edn/read-string (second (re-find #"^(\d+)cm$" v)))
        hgt-in (edn/read-string (second (re-find #"^(\d+)in$" v)))]
    (or (and hgt-cm (>= hgt-cm 150) (<= hgt-cm 193))
        (and hgt-in (>= hgt-in 59) (<= hgt-in 76)))))

;cid (Country ID) - ignored, missing or not.

(def key-validations
  {
   ;byr (Birth Year) - four digits; at least 1920 and at most 2002.
   :byr #(valid-yr % 1920 2002),
   ;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
   :iyr #(valid-yr % 2010 2020),
   ;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
   :eyr #(valid-yr % 2020 2030),
   :hgt valid-hgt,
   ;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
   :hcl #(re-find #"^#[0-9a-f]{6}$" %),
   ;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
   :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"},
   ;pid (Passport ID) - a nine-digit number, including leading zeroes.
   :pid #(re-find #"^\d{9}$" %)})

(defn valid-id2 [id]
  (= (count key-validations)
     (->> (dissoc id :cid)
          (filter (fn [[k v]] ((key-validations k) v)))
          count)))

(defn part2 []
  (->> input
       (map parse-id)
       (filter valid-id2)
       count))

;(println "part 1: " (part1))
;(println "part 2: " (part2))