(ns qrcode.matrix
  (:require [qrcode.utils :as utils]))

(defn create-matrix
  "create a vector of vectors (2d matrix)"
  ([size] (create-matrix size size nil))
  ([rows cols] (create-matrix rows cols nil))
  ([rows cols value]
     (vec
      (for [row (range rows)]
        (vec
         (for [col (range cols)]
           value))))))

(defn set-row-col
  "set value at row,col in matrix. row,col are zero based"
  [matrix r c value]
  (let [row (matrix r)]
    (assoc matrix r (assoc row c value))))

(defn get-row-col
  "get value at row,col in matrix. row,col are zero based"
  [matrix r c]
  (let [row (matrix r)]
    (row c)))

(defn row-col-from-pos
  "determine row,col from zero-based 'pos' in matrix with 'rows' rows and 'cols' columns"
  [pos rows cols]
  [(int (/ pos cols)) (int (mod pos cols))])

(defn pos-from-row-col
  "determine pos from zero-based 'row,col' in matrix"
  [r c rows cols]
  (+ (* r cols) c))

(defn set-position
  "set value at zero-based position in matrix"
  [matrix pos value]
  (let [rows (count matrix)
        cols (count (first matrix))
        [r c] (row-col-from-pos pos rows cols)]
    (set-row-col matrix r c value)))

(defn get-position
  "get value at zero-based position in matrix"
  [matrix pos]
  (let [rows (count matrix)
        cols (count (first matrix))
        [r c] (row-col-from-pos pos rows cols)]
    (get-row-col matrix r c)))

(defn set-pv-pairs
  "take sequence of [position value] pairs and set each position in matrix to corresponding value"
  [matrix pv-pairs]
  (if-let [[p v] (first pv-pairs)]
    (recur (set-position matrix p v) (rest pv-pairs))
    matrix))

(defn set-column
  "set the column starting at r,c to the values given. Stop when either the column or the values is/are exhausted"
  [matrix r c values]
  (let [rows (count matrix)
        cols (count (first matrix))
        positions (range (pos-from-row-col r c rows cols) (* rows cols) cols)]
    (set-pv-pairs matrix (utils/position-value-pairs positions values))))

(defn set-row
  "set the row starting at r,c to the values given. Stop when either the row or the values is/are exhausted"
  [matrix r c values]
  (let [rows (count matrix)
        cols (count (first matrix))
        positions (range (pos-from-row-col r c rows cols) (* (inc r) cols))]
    (set-pv-pairs matrix (utils/position-value-pairs positions values))))

(defn set-rectangle
  "set the sub-matrix starting at r,c that matches rectangle's dimensions to the values of rectangle"
  [matrix r c rectangle]
  (let [m-rows (count matrix)
        m-cols (count (first matrix))
        r-rows (count rectangle)
        r-cols (count (first rectangle))
        pv-pairs (vec (for [row (range r-rows) col (range r-cols)]
                        [(pos-from-row-col (+ row r) (+ col c) m-rows m-cols) (get-row-col rectangle row col)]))]
    (set-pv-pairs matrix pv-pairs)))

(defn rotate
  "rotate a matrix"
  [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (vec (for [col (range cols)]
           (vec (for [row (range rows)]
                  (get-row-col matrix row col)))))))
