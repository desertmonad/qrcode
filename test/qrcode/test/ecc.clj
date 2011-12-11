(ns qrcode.test.ecc
  (:use clojure.test)
  (:use qrcode.ecc))

(deftest merge-blocks*-test
  (let [blocks [[1 2 3] [4 5 6] [7 8 9 10] [11 12 13 14]]
        expected [1 4 7 11 2 5 8 12 3 6 9 13 10 14]]
    (is (= expected (merge-blocks* blocks)))))

(deftest merge-blocks-test
  (let [blocks [[[1 2 3] [15 16 17]] [[4 5 6] [18 19 20]] [[7 8 9 10] [21 22 23]] [[11 12 13 14] [24 25 26]]]
        expected [1 4 7 11 2 5 8 12 3 6 9 13 10 14 15 18 21 24 16 19 22 25 17 20 23 26]]
    (is (= expected (merge-blocks blocks)))))

(deftest generate-ecc-test
  (let [data-map {:version 2,
                  :type :8bit,
                  :size 34,
                  :error-level :L,
                  :original "http://clockpartners.com",
                  :original-length 24,
                  :encoded [65 134 135 71 71 3 162 242 246 54 198 246 54 183 6 23 39
                            70 230 87 39 50 230 54 246 208 236 17 236 17 236 17 236 17],
                  :encoded-length 34}
        expected {:error-level :L,
                  :ecc-encoded [65 134 135 71 71 3 162 242 246 54 198 246 54 183 6 23 39 70 230 87 39 50
                                230 54 246 208 236 17 236 17 236 17 236 17 25 219 31 163 78 171 73 52 128 38],
                  :encoded [65 134 135 71 71 3 162 242 246 54 198 246 54 183 6 23 39
                            70 230 87 39 50 230 54 246 208 236 17 236 17 236 17 236 17],
                  :original-length 24,
                  :version 2,
                  :size 34,
                  :original "http://clockpartners.com",
                  :type :8bit,
                  :encoded-length 34,
                  :ecc-encoded-length 44}]
    (is (= expected (generate-ecc data-map)))))
