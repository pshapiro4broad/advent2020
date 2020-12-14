(ns day14
  (:require [clojure.string :as str]))

(def input
  (-> "src/day14-input.txt"
      slurp
      str/split-lines))

;mem[23672] = 853256
;mask = 11000011010XX01101X000XX10111001X11X

(defn apply-mask [mask value]
  (if mask
    (let [mask-val (bit-shift-left 1 (dec (count mask)))]
      (apply-mask (next mask)
                  (case (first mask)
                    \X value
                    \0 (bit-and-not value mask-val)
                    \1 (bit-or value mask-val))))
    value))

(defn initialize [program update-memory]
  (loop [lines program
         mask nil
         memory {}]
    (if lines
      (let [line (str/split (first lines) #" = ")]
        (if (= (first line) "mask")
          (recur (next lines) (last line) memory)
          (let [address (read-string (nth (str/split (first line) #"[\[\]]") 1))
                value (read-string (last line))]
            (recur (next lines) mask
                   (update-memory memory address mask value)))))
      (reduce + (vals memory)))))

(defn part1 []
  (initialize input
              (fn [memory address mask value]
                (assoc memory address (apply-mask mask value)))))

(defn apply-mem-mask [mask value]
  (loop [mask mask
         values [value]]
    (if mask
      (let [mask-val (bit-shift-left 1 (dec (count mask)))]
        (recur (next mask)
               (case (first mask)
                 \X (->> values
                         (map (fn [x] [(bit-and-not x mask-val)
                                       (bit-or x mask-val)]))
                         flatten)
                 \0 values
                 \1 (map #(bit-or % mask-val) values))))
      values)))

(defn part2 []
  (initialize input
              (fn [memory address mask value]
                (->> (apply-mem-mask mask address)
                     (map (fn [x] {x value}))
                     (reduce merge memory)))))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )