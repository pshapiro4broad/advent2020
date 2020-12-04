(ns day4
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))
(def input
  (str/split
    (-> "src/day4-input.txt"
        slurp)
    #"\n\n"))

(defn parse-id [text]
  (->> (str/split text #"[ \n]")
       (map #(str/split % #":"))
       flatten
       (apply hash-map)
       clojure.walk/keywordize-keys))

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

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
(defn valid-byr [v]
  (let [yr (edn/read-string (re-find #"^\d{4}$" v))]
    (and yr (>= yr 1920) (<= yr 2002))))

;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(defn valid-iyr [v]
  (let [yr (edn/read-string (re-find #"^\d{4}$" v))]
    (and yr (>= yr 2010) (<= yr 2020))))

;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(defn valid-eyr [v]
  (let [yr (edn/read-string (re-find #"^\d{4}$" v))]
    (and yr (>= yr 2020) (<= yr 2030))))

;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
(defn valid-hgt [v]
  (let [hgt-cm (edn/read-string (second (re-find #"^(\d+)cm$" v)))
        hgt-in (edn/read-string (second (re-find #"^(\d+)in$" v)))]
    (or (and hgt-cm (>= hgt-cm 150) (<= hgt-cm 193))
        (and hgt-in (>= hgt-in 59) (<= hgt-in 76)))))

;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(defn valid-hcl [v]
  (re-find #"^#[0-9a-f]{6}$" v))

;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(defn valid-ecl [v]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v))

;pid (Passport ID) - a nine-digit number, including leading zeroes.
(defn valid-pid [v]
  (re-find #"^\d{9}$" v))

;cid (Country ID) - ignored, missing or not.

(def key-validations
  {:byr valid-byr,
   :iyr valid-iyr,
   :eyr valid-eyr,
   :hgt valid-hgt,
   :hcl valid-hcl,
   :ecl valid-ecl,
   :pid valid-pid})

(defn valid-id2 [id]
  (= (count key-validations)
     (count
       (filter (fn [[k v]] ((key-validations k) v))
               (dissoc id :cid)))))

(defn part2 []
  (->> input
       (map parse-id)
       (filter valid-id2)
       count))

(println "part 1: " (part1))
(println "part 2: " (part2))