(ns qrcode.score
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.matrix :as matrix]))

(def n1 3)
(def n2 3)
(def n3 40)
(def n4 10)

(def adjacent-size-cutoff 5)

(defn sequences-of-same [matrix]
  (for [row matrix]
    (loop [results []
           row row]
      (if (seq row)
        (let [target (first row)
              n  (count (take-while #(= % target) row))]
          (recur (conj results n) (drop n row)))
        results))))

(defn sequences-score [matrix]
  (count (filter #(>= % adjacent-size-cutoff) (flatten (sequences-of-same matrix)))))

(defn adjacent-color-score [matrix reverse]
  (* n1
     (+ (sequences-score matrix)
        (sequences-score reverse))))

(defn block-add [target total row]
  (vec (map #(let [factor (if (= %2 target) 1 0)] (* factor (inc %1))) total row)))

(defn block-score-reduction [matrix target]
  (let [size (count matrix)]
    (loop [results [(vec (take size sequences/zeros))]
           m matrix]
      (if (seq m)
        (recur (conj results (block-add target (last results) (first m))) (rest m))
        results))))

(defn block-score [matrix target]
  (apply + (map #(apply + %) (block-score-reduction matrix target))))

(defn block-color-score [matrix reverse]
  (+
   (block-score matrix 1)
   (block-score matrix 0)
   (block-score reverse 1)
   (block-score reverse 0)))

(defn is-position-detection-pattern? [[x1 x2 x3 x4 x5 x6 x7]]
  (and (= x1 x3 x4 x5 x7 1) (= x2 x6 0)))

(defn position-detection-pattern-score [matrix reverse]
  (* n3 (+
         (count (filter true? (flatten (for [row matrix]  (map is-position-detection-pattern? (partition 7 1 row))))))
         (count (filter true? (flatten (for [row reverse] (map is-position-detection-pattern? (partition 7 1 row)))))))))

(defn dark-light-proportion-score [matrix]
  (let [size (count matrix)
        total (* size size)
        expected (int (/ total 2))
        light (count (flatten (for [row matrix] (filter zero? row))))]
    (* n4 (int (Math/abs (- expected light))))))


(defn total-score [matrix]
  (let [reverse (matrix/rotate matrix)]
    (+
     (dark-light-proportion-score matrix)
     (position-detection-pattern-score matrix reverse)
     (block-color-score matrix reverse)
     (adjacent-color-score matrix reverse))))


