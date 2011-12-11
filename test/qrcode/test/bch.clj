(ns qrcode.test.bch
  (:use clojure.test)
  (:use qrcode.bch))

(deftest divide-test
  (is (= 3220 (divide 7 7973 12)))
  (is (= 3177 (divide 40 7973 12))))
