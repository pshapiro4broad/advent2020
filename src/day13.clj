(ns day13
  (:require [clojure.string :as str]))

(def input
  (-> "src/day13-input.txt"
      slurp
      str/split-lines))

(defn part1 []
  (let [target (read-string (first input))
        lines (-> (last input)
                  (str/split #","))]
    (->> lines
         (remove #(= % "x"))
         (map read-string)
         (map (fn [x] [x (* x (inc (quot target x)))]))
         (sort-by last <)
         first
         (#(* (first %) (- (last %) target))))))

(defn abs [n] (max n (- n)))

; https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure
(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)                   ; p = prod / n_i
                        egcd (extended-gcd p n_i)           ; Extended gcd
                        inv_p (second egcd)]                ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]       ; Replaces the Python for loop to sum
    ; (map vector n a) is same as
    ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                                   ; Result line

(defn part2 []
  (let [lines (-> (last input)
                  (str/split #","))]
    (->> (range (count lines))
         (map (fn [x] [x (nth lines x)]))
         (remove #(= (last %) "x"))
         (map (fn [[i bus]] (let [bus (read-string bus)]
                              [bus (- bus i)])))
         (#(chinese-remainder (map first %) (map last %))))))


(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
