(ns day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day20-input.txt"
      slurp
      (str/split #"\n\n")))

; tiles are 10x10
(def tile-size 10)
(def tile-edges
  (let [edges [(for [x (range tile-size)] [x 0])
               (for [y (range tile-size)] [0 y])
               (for [y (range tile-size)] [(dec tile-size) y])
               (for [x (range tile-size)] [x (dec tile-size)])]]
    (into edges (map reverse edges))))

(defn edges [tile]
  (for [edge tile-edges]
    (apply str
           (for [[x y] edge]
             (nth (nth tile y) x)))))

(defn all-edges
  "Create a set of all edges in tiles, skipping tile skip"
  [tiles skip]
  (into #{} (flatten (map #(and (not= skip %) (% :edges)) tiles))))

(defn parse-tile [text]
  (let [lines (str/split-lines text)
        tile-id (->> (first lines) (re-find #"Tile (\d+):") fnext read-string)
        tile (rest lines)]
    {:id tile-id
     :edges (edges tile)
     :tile tile}))

(defn part1 []
  (let [parsed (map parse-tile input)]
    (->> parsed
         ; 4 matching edges means we have a corner (2 matching sides x 2 different rotations)
         (filter #(= 4 (count (filter (all-edges parsed %) (% :edges)))))
         (map :id)
         (reduce *))))

;; what a mess. If I had converted tiles to (set of pos) instead of list of strings, this would've been easier.
(defn rotate [tile]
  (for [x (range (count (first tile)))]
    (apply str
           (for [y (range (count tile))]
             (nth (nth tile (- (dec tile-size) y)) x)))))

(defn flip [tile]
  (for [x (range (count (first tile)))]
    (apply str
           (for [y (range (count tile))]
             (nth (nth tile y) x)))))

(defn edge-to-top [{tile :tile edges :edges} edge]
  (case (.indexOf edges edge)
    0 tile
    1 (flip tile)
    2 (rotate (rotate (rotate tile)))
    3 (rotate (rotate (rotate (flip tile))))
    4 (rotate (flip tile))
    5 (rotate tile)
    6 (rotate (rotate (flip tile)))
    7 (rotate (rotate tile))
    ))

(defn find-tile [tiles edge]
  (filter (fn [{edges :edges}] (contains? (set edges) edge)) tiles))

; from running part1, I know this is a corner tile
(def corner-tile-id 1091)
(defn get-tile [tiles tile-id]
  (first (filter (fn [{id :id}] (= id tile-id)) tiles)))

(defn position-corner [tiles]
  (let [corner (get-tile tiles corner-tile-id)
        all-tiles (remove #(= % corner) tiles)
        neighbors (fn [edges] (find-tile all-tiles edges))]
    ; rotate tile until left and right are not connected
    (loop [tile (corner :tile)]
      (let [edges (edges tile)
            top-neighbors (neighbors (first edges))
            left-neighbors (neighbors (second edges))]
        (if (= () top-neighbors left-neighbors)
          [corner tile]
         (recur (rotate tile)))))))

(defn position-below [tiles above above-tile]
  (let [all-tiles (remove #(= above %) tiles)
        edge (last above-tile)
        below (first (filter #((set (% :edges)) edge) all-tiles))]
    [below (edge-to-top below edge)]))

(defn position-right [tiles left left-tile]
  (let [[right right-tile] (position-below tiles left (rotate left-tile))]
    [right (rotate (rotate (rotate right-tile)))]))

;; tiles are in a 12 x 12 grid
(def num-tiles 12)

(defn position-all [tiles]
  (loop [positioned {[0 0] (position-corner tiles)}
         index 1]
    (if (= index (* num-tiles num-tiles))
      positioned
      (let [x (mod index num-tiles)
            y (quot index num-tiles)]
        (recur (assoc positioned
                [x y]
                (if (zero? x)
                  (let [above (positioned [x (dec y)])]
                    (position-below tiles (first above) (last above)))
                  (let [left (positioned [(dec x) y])]
                    (position-right tiles (first left) (last left)))))
               (inc index))))))

(defn tile-to-set [tile top left]
  (->> (for [x (range 1 (dec tile-size))]
         (set (for [y (range 1 (dec tile-size))
                    :when (= \# (nth (nth tile y) x))]
                [(+ top (dec x)) (+ left (dec y))])))
       (reduce set/union)))

;; size of tile minus its border
(def inside-size 8)

(def nessie
  '("                  # "
     "#    ##    ##    ###"
     " #  #  #  #  #  #   "))

(defn monster-to-set [monster]
  (->> (for [x (range (count (first monster)))]
         (set (for [y (range (count monster))
                    :when (= \# (nth (nth monster y) x))]
                [x y])))
       (reduce set/union)))

(defn translate [points top left]
  (->> points
      (map (fn [[x y]] [(+ x top) (+ y left)]))
       set))

(defn rotate2 [points width]
  (->> points
       (map (fn [[x y]] [(- width y) x]))
       set))

(defn flip2 [points]
  (->> points
       (map (fn [[x y]] [y x]))
       set))

; for debugging
(defn draw [image h w]
  (doall
    (for [y (range h)]
      (do
        (apply print
               (for [x (range w)]
                 (if (image [x y]) \# \.)))
        (println))))
  nil)

;; 12x12 tiles
(defn part2 []
  (let [tiles (map parse-tile input)
        monster (monster-to-set nessie)
        positioned (position-all tiles)
        image (->> positioned
                   (map (fn [[[x y] [_ tile]]] (tile-to-set tile (* x inside-size) (* y inside-size))))
                   (reduce set/union))
        ;; monsters in all possible orientations
        monsters ((juxt identity flip2
                        #(rotate2 % 3) #(flip2 (rotate2 % 3))
                        #(rotate2 (rotate2 % 3) 20) #(flip2 (rotate2 (rotate2 % 3) 20))
                        #(rotate2 (rotate2 (rotate2 % 3) 20) 3) #(flip2 (rotate2 (rotate2 (rotate2 % 3) 20) 3))
                        ) monster)]
    (->> monsters
         (map #(->> (for [x (range (* inside-size num-tiles))]
                      (for [y (range (* inside-size num-tiles))
                            :let [monster (translate % x y)]
                            :when (set/subset? monster image)]
                        monster))))
         flatten
         (reduce set/union)
         (set/difference image)
         count)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )