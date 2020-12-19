(ns day19
  (:require [clojure.string :as str]))

;0: 1 2
;1: "a"
;2: 1 3 | 3 1
(defn parse-rule [text]
  (let [s1 (str/split text #": ")
        or-exp (re-find #"(.+) \| (.+)" (last s1))
        term-exp (re-find #"\"([ab])\"" (last s1))]
    [(read-string (first s1))
     (cond
       term-exp (last term-exp)
       or-exp (for [e (rest or-exp)
                    :let [s (str/split e #" ")]]
                (map read-string s))
       :else (-> (last s1)
                 (str/split #" ")
                 (->> (map read-string))))]))

(defn parse-input [input]
  (let [[rules messages] input]
    [(->> rules
          (map parse-rule)
          (into {}))
     messages]))

(def input
  (-> "src/day19-input.txt"
      slurp
      (str/split #"\n\n")
      (->> (map #(str/split-lines %)))
      parse-input))

(defn valid-message2 [rule-map rules message]
  (if rules
    (let [rule (rule-map (first rules))]
      (cond
        (seq? (first rule))
        (or (valid-message2 rule-map (flatten (conj (next rules) (first rule))) message)
            (valid-message2 rule-map (flatten (conj (next rules) (last rule))) message))

        (seq? rule)
        (valid-message2 rule-map (flatten (conj (next rules) rule)) message)

        (= (first message) (first rule))
        (valid-message2 rule-map (next rules) (next message))

        :else false))
    (empty? message)))

(defn valid-message? [rule-map message]
  (valid-message2 rule-map (rule-map 0) (seq message)))

(defn part1 []
  (let [[rule-map messages] input]
    (->> messages
         (filter #(valid-message? rule-map %))
         count)))

; As you look over the list of messages, you realize your matching rules aren't quite right.
; To fix them, completely replace rules 8: 42 and 11: 42 31 with the following:
;
; 8: 42 | 42 8
; 11: 42 31 | 42 11 31
(defn part2 []
  (let [[rule-map messages] input
        rule-map (assoc rule-map 8 '((42) (42 8)) 11 '((42 31) (42 11 31)))]
    (->> messages
         (filter #(valid-message? rule-map %))
         count)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )