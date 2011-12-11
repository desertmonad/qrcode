(ns qrcode.test.utils
  (:use clojure.test)
  (:use qrcode.utils))

(deftest create-position-value-pairs
  (let [positions (range 8 -1 -1)
        values    (range 9)]
    (is (= [[8 0] [7 1] [6 2] [5 3] [4 4] [3 5] [2 6] [1 7] [0 8]] (position-value-pairs positions values)))))

(deftest combination-of-values
  (let [values [1 2 3]]
    (is (= [[1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3]] (combinations values)))))

(deftest encode-n-to-bits
  (is (= [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1] (n-to-bits (long 5))))
  (is (= [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1] (n-to-bits 5)))
  (is (= [0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1] (n-to-bits (short 5))))
  (is (= [0 0 0 0 0 1 0 1] (n-to-bits (byte 5))))
  (is (= [0 0 1 0 1] (n-to-bits 5 5)))
  (is (= [1 0 1] (n-to-bits 5 3))))


(deftest decode-bits-t0-n
  (is (= 5 (bits-to-n [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1])))
  (is (= 5 (bits-to-n [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1])))
  (is (= 5 (bits-to-n [0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1])))
  (is (= 5 (bits-to-n [0 0 0 0 0 1 0 1])))
  (is (= 5 (bits-to-n [1 0 1]))))

(deftest most-significant-bit
  (is (= 1  (msb 1)))
  (is (= 5  (msb 16)))
  (is (= 32 (msb 0xFFFFFFFF)))
  (is (= 63 (msb 0x7FFFFFFFFFFFFFFF))))
