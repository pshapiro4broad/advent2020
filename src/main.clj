(ns main
  (:require [clojure.java.io :as io]))

(defn run []
  (->> (io/file "./src")
       file-seq
       (map #(re-find #"^day(\d+).clj$" (.getName %)))
       (filter some?)
       (map last)
       (map read-string)
       sort
       (map #(do
               (require (symbol (str "day" %)))
               (println "day" %)
               (println "part 1:" ((eval (symbol (str "day" % "/part1")))))
               (println "part 2:" ((eval (symbol (str "day" % "/part2"))))))))
  )