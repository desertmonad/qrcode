(ns qrcode.mask
  (:require [qrcode.pattern :as pattern])
  (:require [qrcode.score :as score])
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.matrix :as matrix]))

(def masks
  [
   [[0 0 0] (fn [r c] (= (mod (+ r c) 2) 0))]
   [[0 0 1] (fn [r c] (= (mod r 2) 0))]
   [[0 1 0] (fn [r c] (= (mod c 3) 0))]
   [[0 1 1] (fn [r c] (= (mod (+ r c) 3) 0))]
   [[1 0 0] (fn [r c] (= (mod (+ (int (/ r 2)) (int (/ c 3))) 2) 0))]
   [[1 0 1] (fn [r c] (= (+ (mod (* r c) 2) (mod (* r c) 3)) 0))]
   [[1 1 0] (fn [r c] (= (mod (+ (mod (* r c) 2) (mod (* r c) 3)) 2) 0))]
   [[1 1 1] (fn [r c] (= (mod (+ (mod (* r c) 3) (mod (+ r c) 2)) 2) 0))]
   ])

(defn apply-mask [{:keys [qrcode version] :as m} [code f]]
  (let [size (count qrcode)
        pattern (pattern/generate-usability-pattern version)]
    (loop [qrcode qrcode
           positions (sequences/valid-bitposition-sequence pattern)]
      (if (seq positions)
        (let [position (first positions)
              [r c] (matrix/row-col-from-pos position size size)
              mask-value (if (f r c) 1 0)
              value (matrix/get-position qrcode position)]
          (recur (matrix/set-position qrcode position (bit-xor value mask-value)) (rest positions)))
        (merge m {:mask-code code :qrcode qrcode})))))

(defn best-mask-score [m1 m2]
  (let [s1 (score/total-score (:qrcode m1))
        s2 (score/total-score (:qrcode m2))]
    (if (< s1 s2) m1 m2)))

(defn find-best-mask [m]
  (reduce best-mask-score (for [mask masks] (apply-mask m mask))))
