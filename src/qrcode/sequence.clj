(ns qrcode.sequence
  (:require [qrcode.matrix :as matrix]))

;;
;; careful -- infinite lazy sequences
;;

(def integers (range))
(def positive (drop 1 integers))
(def zeros    (cycle [0]))

(def timing-pattern    (cycle [1 0]))
(def padding-codewords (cycle [236 17]))

;;
;; generate a lazy sequence of bit positions that bits from qrcode data/ecc go for a qrcode of size n
;;

(defn bitposition-sequence [n] {:pre [(or (and (< n 7) (even? n)) (and (>= n 7) (zero? (mod (dec n) 4))))]}
  (let [stop (* n (if (>= n 7) (dec n) n))
        p    n
        f    -]
    (for [x (range 0 stop)]
      (let [col (- n (* (int (/ x (* n 2))) 2))
            col (if (and (>= n 7) (<= col 7)) (dec col) col)
            group (int (/ x (* n 2)))
            [p f] (if (zero? (mod (int (/ x (* n 2))) 2)) [n -] [1 +])
            row (f p (mod (int (/ x 2)) n))
            step (mod x 2)]
        (- (+ (* (dec row) n) col) step)))))

(defn valid-bitposition? [pattern position]
  (let [row (int (/ position (count pattern)))
        col (int (mod position (count pattern)))]
    (nil? (matrix/get-row-col pattern row col))))

(defn valid-bitposition-sequence [pattern]
  (filter #(valid-bitposition? pattern %) (map dec (bitposition-sequence (count pattern)))))


;;
;; sequence of positions for format data bits to be placed
;;
;; primary positions are around upper left position detection pattern
;;
;; secondary positions are under upper right position detection
;; pattern and beside lower left position detection pattern
;;

(def number-positions-in-group 8)
(def row 9)
(def col 9)

(defn primary-format-row-positions [size]
  (let [timing-position (+ (* size (dec row)) 6)
        start (* size (dec row))
        stop  (+ start number-positions-in-group)]
    (filter #(not (= % timing-position)) (range start stop))))

(defn primary-format-column-positions [size]
  (let [timing-position (+ (* size 6) (dec col))
        start (dec col)
        stop (* size (inc number-positions-in-group))]
    (filter #(not (= % timing-position)) (range start stop size))))

(defn primary-format-sequence [version]
  (let [size (+ 17 (* version 4))]
    (vec (flatten (apply conj (reverse (primary-format-column-positions size)) (reverse (primary-format-row-positions size)))))))

(defn secondary-format-row-positions [size]
  (let [start (- (* size row) number-positions-in-group)
        stop  (* size row)]
    (range start stop)))

(defn secondary-format-column-positions [size]
  (let [start-row (- size (dec number-positions-in-group))
        start (+ (* start-row size) (dec col))
        stop (* size size)]
    (range start stop size)))

(defn secondary-format-sequence [version]
  (let [size (+ 17 (* version 4))]
    (vec (flatten (apply conj (secondary-format-row-positions size) (secondary-format-column-positions size))))))

