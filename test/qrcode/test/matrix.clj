(ns qrcode.test.matrix
  (:require [qrcode.utils :as utils])
  (:use clojure.test)
  (:use qrcode.matrix))

(deftest matrix-creation
  (is (= [[nil nil nil] [nil nil nil] [nil nil nil]] (create-matrix 3)))
  (is (= [[nil]]                                     (create-matrix 1)))
  (is (= [[nil nil nil]]                             (create-matrix 1 3)))
  (is (= [[\a \a \a] [\a \a \a]]                     (create-matrix 2 3 \a)))
  (is (= [[1 1 1] [1 1 1] [1 1 1]]                   (create-matrix 3 3 1))))

(deftest set-value-at-row-col
  (let [size 3
        matrix (create-matrix size)]
    (is (= [[nil nil nil] [nil \a nil] [nil nil nil]] (set-row-col matrix 1 1 \a)))
    (is (= [[\b nil nil] [nil nil nil] [nil nil nil]] (set-row-col matrix 0 0 \b)))
    (is (= [[nil nil nil] [nil nil nil] [\c nil nil]] (set-row-col matrix 2 0 \c)))
    (is (= [[nil nil nil] [nil nil nil] [nil nil \d]] (set-row-col matrix (dec size) (dec size) \d)))))

(deftest get-value-at-row-col
  (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
    (is (= 1 (get-row-col matrix 0 0)))
    (is (= 6 (get-row-col matrix 1 2)))))


(deftest row-col-from-position
  (let [rows 10
        cols 10]
    (is (= [0 0] (row-col-from-pos 0 rows cols)))
    (is (= [0 4] (row-col-from-pos 4 rows cols)))
    (is (= [2 0] (row-col-from-pos 20 rows cols)))
    (is (= [9 9] (row-col-from-pos 99 rows cols)))))

(deftest position-from-row-col
  (let [rows 10
        cols 10]
    (is (= 0  (pos-from-row-col 0 0 rows cols)))
    (is (= 4  (pos-from-row-col 0 4 rows cols)))
    (is (= 20 (pos-from-row-col 2 0 rows cols)))
    (is (= 99 (pos-from-row-col 9 9 rows cols)))))

(deftest set-value-at-position
  (let [size 3
        matrix (create-matrix 3)]
    (is (= [[\a nil nil] [nil nil nil] [nil nil nil]] (set-position matrix 0 \a)))
    (is (= [[nil nil nil] [nil nil nil] [nil nil \b]] (set-position matrix 8 \b)))))

(deftest get-value-at-position
  (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
    (is (= 1 (get-position matrix 0)))))


(deftest set-pos-values
  (let [matrix (create-matrix 3)
        pv-pairs (utils/position-value-pairs (range 8 -1 -1) (range 9))]
    (is (= [[8 7 6] [5 4 3] [2 1 0]] (set-pv-pairs matrix pv-pairs)))
    (is (= matrix (set-pv-pairs matrix nil)))
    (is (= matrix (set-pv-pairs matrix [])))))

(deftest set-column-values
  (let [matrix (create-matrix 3)]
    (is (= [[nil nil 1] [nil nil 2] [nil nil 3]] (set-column matrix 0 2 [1 2 3])))
    (is (= [[nil nil nil] [nil nil 1] [nil nil 2]] (set-column matrix 1 2 [1 2])))
    (is (= [[nil nil nil] [nil nil nil] [nil nil 1]] (set-column matrix 2 2 [1])))))

(deftest set-row-values
  (let [matrix (create-matrix 3)]
    (is (= [[1 2 3] [nil nil nil] [nil nil nil]] (set-row matrix 0 0 [1 2 3])))
    (is (= [[nil nil nil] [nil 1 2] [nil nil nil]] (set-row matrix 1 1 [1 2])))
    (is (= [[nil nil nil] [nil nil nil] [nil nil 1]] (set-row matrix 2 2 [1])))))

(deftest set-rectangle-values
  (let [matrix (create-matrix 4 4 0)
        rectangle (create-matrix 3 3 1)]
    (is (= [[1 1 1 0] [1 1 1 0] [1 1 1 0] [0 0 0 0]] (set-rectangle matrix 0 0 rectangle)))
    (is (= [[0 0 0 0] [0 1 1 1] [0 1 1 1] [0 1 1 1]] (set-rectangle matrix 1 1 rectangle)))))

(deftest rotate-matrix
  (let [matrix [[1 2 3] [4 5 6] [7 8 9]]]
    (is (= [[1 4 7] [2 5 8] [3 6 9]] (rotate matrix)))
    (is (= [[1] [2] [3]] (rotate [[1 2 3]])))))
