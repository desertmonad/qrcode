(ns qrcode.test.galois
  (:use [qrcode.galois])
  (:use [clojure.test]))

(defn only-contains [valid values]
  (let [is-valid-value (set valid)]
    (every? #(is-valid-value %) values)))

(deftest all
  (is (= (map #(antilog (log %)) (range 1 256)) (range 1 256)))

  (let [in-field (range 256)]
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 256)] (add x y))))))
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 256)] (subtract x y))))))
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 256)] (multiply x y))))))
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 256) z (range 256)] (multiply x y z))))))
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 1 256) z (range 1 256)] (divide x y))))))
    (is (= true (only-contains in-field (set (for [x (range 256) y (range 1 256) z (range 1 256)] (divide x y z)))))))

  (is (= (generator-polynomial 7) [1 127 122 154 164 11 68 117]))

  (let [numer [16 32 12 86 97 128 236 17 236 17 236 17 236 17 236 17 0 0 0 0 0 0 0]
        denom (generator-polynomial 7)] 
    (is (= (poly-divide numer denom) [97 13 169 7 28 175 7]))))
