(ns qrcode.pattern
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.definitions :as defs])
  (:require [qrcode.matrix :as matrix])
  (:require [qrcode.version :as version])
  (:require [qrcode.utils :as utils]))

(def position-detection
  [[1 1 1 1 1 1 1]
   [1 0 0 0 0 0 1]
   [1 0 1 1 1 0 1]
   [1 0 1 1 1 0 1]
   [1 0 1 1 1 0 1]
   [1 0 0 0 0 0 1]
   [1 1 1 1 1 1 1]])

(def separator-zone [0 0 0 0 0 0 0 0])

(def format-zone [1 1 1 1 1 1 1 1 1])

(def alignment
  [[1 1 1 1 1]
   [1 0 0 0 1]
   [1 0 1 0 1]
   [1 0 0 0 1]
   [1 1 1 1 1]])

;;
;; draw the format zones
;;
(defn draw-format-zone [matrix]
  (let [n (count matrix)]
    (-> matrix
        (matrix/set-row 8 0 format-zone)
        (matrix/set-column 0 8 format-zone)
        (matrix/set-row 8 (- n 8) format-zone)
        (matrix/set-column (- n 8) 8 format-zone))))

(defn draw-timing-patterns [matrix]
  (let [n (count matrix)]
    (-> matrix
        (matrix/set-row 6 0 (take n sequences/timing-pattern))
        (matrix/set-column 0 6 (take n sequences/timing-pattern)))))


;;
;; functions to draw the alignment patterns
;;

(defn alignment-pattern-exclusions [values]
  (let [f (first values)
        l (last values)]
    [[f f] [f l] [l f]]))

(defn alignment-pattern-points [n]
  (let [values (defs/alignment-pattern-coordinates n)
        exclusions (set (alignment-pattern-exclusions values))]
    (map (fn [[x y]] [(- x 2) (- y 2)]) (filter #(not (contains? exclusions %)) (utils/combinations values)))))

(defn draw-alignment-patterns [matrix]
  (loop [m matrix
         positions (alignment-pattern-points (count m))]
    (if (seq positions)
      (let [[r c] (first positions)]
        (recur (matrix/set-rectangle m r c alignment) (rest positions)))
      m)))

;;
;; functions to draw the position detection patterns
;;

(defn position-detection-points [matrix]
  (let [size (count matrix)
        r-size  (count position-detection)
        offset (- size r-size)]
    [[0 0] [offset 0] [0 offset]]))

(defn draw-position-detection-patterns [matrix]
  (loop [m matrix
         points (position-detection-points m)]
    (if (seq points)
      (let [[r c] (first points)]
        (recur (matrix/set-rectangle m r c position-detection) (rest points)))
      m)))


(defn draw-separator-zone [matrix]
  (let [n (count matrix)]
    (-> matrix
        (matrix/set-column 0 7 separator-zone)
        (matrix/set-column (- n 8) 7 separator-zone)
        (matrix/set-column 0 (- n 8) separator-zone)
        (matrix/set-row 7 0 separator-zone)
        (matrix/set-row 7 (- n 8) separator-zone)    
        (matrix/set-row (- n 8) 0 separator-zone))))

(def primary-version-sequence (vec (range 17 -1 -1)))
(def secondary-version-sequence [17 11 5 16 10 4 15 9 3 14 8 2 13 7 1 12 6 0])

(defn primary-version-information [version-data]
  (matrix/set-pv-pairs (matrix/create-matrix 6 3) (utils/position-value-pairs primary-version-sequence version-data)))

(defn secondary-version-information [version-data]
  (matrix/set-pv-pairs (matrix/create-matrix 3 6) (utils/position-value-pairs secondary-version-sequence version-data)))

(defn draw-version-information [matrix version]
  (if (> version 6)
    (let [size (count matrix)
          [pr pc] [0 (- size 11)]
          [sr sc] [(- size 11) 0]
          version-data (version/encode version)]
      (-> matrix
        (matrix/set-rectangle pr pc (primary-version-information version-data))
        (matrix/set-rectangle sr sc (secondary-version-information version-data))))
    matrix))

;;
;; function to generate a bit map of positions that are usable for
;; data. If position can contain data it will be nil.  Used to walk
;; through the bitpos-sequence above looking for next position for
;; data/ecc
;;
;;

(def generate-usability-pattern
  (memoize
   (fn [version]
     (let [size (+ (* version 4) 17)]
       (-> (matrix/create-matrix size)
           (draw-format-zone)
           (draw-timing-patterns)
           (draw-alignment-patterns)
           (draw-position-detection-patterns)
           (draw-separator-zone)
           (draw-version-information version))))))
