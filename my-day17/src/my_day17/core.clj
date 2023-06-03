(ns my-day17.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-plan [input z]
  (let [lines (str/split-lines input)
        y (count lines)
        x (count (first lines))
        y (if (zero? x) 0 y)
        cell-states (for [ix (range x), iy (range y)] [(= \# (nth (nth lines iy) ix)) ix iy])
        active-cells (filter first cell-states)
        active-cells (map rest active-cells)
        active-cells (map #(conj (vec %) z) active-cells)
        ]
    {:dimensions [x y]
     :map        (into #{} active-cells)}))

(defn parse-input [input]
  (parse-plan input 0))

(defn neighbors-of [[x y z :as cell]]
  (let [neighbors (for [ix (range (- x 1) (+ x 2))
                        iy (range (- y 1) (+ y 2))
                        iz (range (- z 1) (+ z 2))]
                    [ix iy iz])
        neighbors (into #{} neighbors)
        neighbors (disj neighbors cell)]
    neighbors))

(defn neighbor-count [cell world]
  (let [neighbors (neighbors-of cell)
        active-neighbors (set/intersection neighbors world)]
    (count active-neighbors)))

(defn remove-cell [cell world]
  (let [n (neighbor-count cell world)]
    (when-not (<= 2 n 3) cell)))

(defn get-cells-for-removal [world]
  (let [removed-cells (for [cell world] (remove-cell cell world))
        removed-cells (filter some? removed-cells)]
    (into #{} removed-cells)))

(defn get-bounds [world]
  (let [xs (map first world)
        xs (if (empty? xs) [0] xs)
        ys (map second world)
        ys (if (empty? ys) [0] ys)
        zs (map #(nth % 2) world)
        zs (if (empty? zs) [0] zs)]
    [[(apply max xs), (apply min xs)]
     [(apply max ys), (apply min ys)]
     [(apply max zs), (apply min zs)]]))

(defn add-cell [cell world]
  (let [n (neighbor-count cell world)]
    (when (= n 3) cell)))

(defn get-new-cells [world]
  (let [[[max-x min-x]
         [max-y min-y]
         [max-z min-z]] (get-bounds world)
        max-x (inc max-x)
        max-y (inc max-y)
        max-z (inc max-z)
        min-x (dec min-x)
        min-y (dec min-y)
        min-z (dec min-z)
        added-cells (for [ix (range min-x (inc max-x))
                          iy (range min-y (inc max-y))
                          iz (range min-z (inc max-z))]
                      (add-cell [ix iy iz] world))
        added-cells (filter some? added-cells)]
    (into #{} added-cells)))

(defn do-turn [world]
  (let [removed (get-cells-for-removal world)
        added (get-new-cells world)]
    (set/union added (set/difference world removed))))

(defn do-turns [times world]
  (loop [n times, world world]
    (if (zero? n)
      world
      (recur (dec n) (do-turn world)))))

(defn solve-1 []
  (let [input (slurp "input")
        world (:map (parse-input input))]
    (count (do-turns 6 world))))

(defn parse-plan-2 [input z w]
  (let [lines (str/split-lines input)
        y (count lines)
        x (count (first lines))
        y (if (zero? x) 0 y)
        cell-states (for [ix (range x), iy (range y)] [(= \# (nth (nth lines iy) ix)) ix iy])
        active-cells (filter first cell-states)
        active-cells (map rest active-cells)
        active-cells (map #(conj (vec %) z w) active-cells)]
    (into #{} active-cells)))

(defn parse-input-2 [input]
  (parse-plan-2 input 0 0))

(defn neighbors-of-4d [[x y z w :as cell]]
  (let [neighbors (for [ix (range (- x 1) (+ x 2))
                        iy (range (- y 1) (+ y 2))
                        iz (range (- z 1) (+ z 2))
                        iw (range (- w 1) (+ w 2))]
                    [ix iy iz iw])
        neighbors (into #{} neighbors)
        neighbors (disj neighbors cell)]
    neighbors))

(defn neighbor-count-4d [cell world]
  (let [neighbors (neighbors-of-4d cell)
        active-neighbors (set/intersection neighbors world)]
    (count active-neighbors)))

(defn remove-cell-4d [cell world]
  (let [n (neighbor-count-4d cell world)]
    (when-not (<= 2 n 3) cell)))

(defn get-cells-for-removal-4d [world]
  (let [removed-cells (for [cell world] (remove-cell-4d cell world))
        removed-cells (filter some? removed-cells)]
    (into #{} removed-cells)))

(defn add-cell-4d [cell world]
  (let [n (neighbor-count-4d cell world)]
    (when (= n 3) cell)))

(defn get-dimension-coords [function world]
  (let [coords (map function world)]
    (if (empty? coords) [0] coords)))

(defn get-bounds-4d [world]
  (let [xs (get-dimension-coords first world)
        ys (get-dimension-coords second world)
        zs (get-dimension-coords #(nth % 2) world)
        ws (get-dimension-coords #(nth % 3) world)]
    [[(apply max xs), (apply min xs)]
     [(apply max ys), (apply min ys)]
     [(apply max zs), (apply min zs)]
     [(apply max ws), (apply min ws)]]))

(defn get-new-cells-4d [world]
  (let [[[max-x min-x]
         [max-y min-y]
         [max-z min-z]
         [max-w min-w]] (get-bounds-4d world)
        added-cells (for [ix (range (dec min-x) (+ max-x 2))
                          iy (range (dec min-y) (+ max-y 2))
                          iz (range (dec min-z) (+ max-z 2))
                          iw (range (dec min-w) (+ max-w 2))]
                      (add-cell-4d [ix iy iz iw] world))
        added-cells (filter some? added-cells)]
    (into #{} added-cells)))

(defn do-turn-2 [world]
  (let [removed (get-cells-for-removal-4d world)
        added (get-new-cells-4d world)]
    (set/union added (set/difference world removed))))

(defn do-turns-2 [times world]
  (loop [n times, world world]
    (if (zero? n)
      world
      (recur (dec n) (do-turn-2 world)))))

(defn solve-2 []
  (let [input (slurp "input")
        world (parse-input-2 input)]
    (count (do-turns-2 6 world))))
