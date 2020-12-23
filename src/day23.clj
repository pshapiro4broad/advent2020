(ns day23
  (:require [clojure.string :as str])
  (:import (java.util LinkedList)))

(def input "364297581")

;; ring
(defn make-ring [] (LinkedList.))
(defn r-peek [ring] (.peekFirst ring))
(defn r-peek-last [ring] (.peekLast ring))
(defn r-pop [ring] (.pop ring))
(defn r-pop-last [ring] (.removeLast ring))
(defn r-push [ring x] (.push ring x))
(defn r-push-last [ring x] (.addLast ring x))
(defn r-length [ring] (.size ring))
(defn r-nth [ring i] (.get ring i))

(defn rotate [ring]
  (let [first (r-peek ring)]
    (r-pop ring)
    (r-push-last ring first)))

(defn rotate-to [ring v]
  (if (= (r-peek ring) v)
    ring
    (do
      (rotate ring)
      (rotate-to ring v))))

(defn r-print [ring]
  (->> (range (r-length ring))
      (map #(r-nth ring %))
      (str/join "")))

(defn parse-input [input]
  (let [ring (make-ring)]
    (doall
      (->> (str/split input #"")
           (map read-string)
           (map #(r-push-last ring %))))
    ring))

(defn find-destination [cup skip]
  (loop [cup (if (= cup 1) 9 (dec cup))]
    (if (skip cup)
      (recur (if (= cup 1) 9 (dec cup)))
      cup)))

(defn move [cups]
  (let [current (r-peek cups)]
    (rotate cups)
    (let [c1 (r-pop cups)
          c2 (r-pop cups)
          c3 (r-pop cups)
          destination (find-destination current #{c1 c2 c3})]
      (rotate-to cups destination)
      (rotate cups)
      (r-push cups c3)
      (r-push cups c2)
      (r-push cups c1)
      (rotate-to cups current)
      (rotate cups))))

(defn part1 []
  (let [cups (parse-input input)]
    (dotimes [_ 100] (move cups))
    (rotate-to cups 1)
    (r-pop cups)
    (r-print cups)))

(defn part2 []
  )

;(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  ;)