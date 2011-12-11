(ns qrcode.format
  (:require [qrcode.bch :as bch])
  (:require [qrcode.utils :as utils]))

;;
;; G(x) = x^10 + x^8 + x^5 + x^4 + x^2 + x + 1
;;
(def gp (utils/bits-to-n [1 0 1 0 0 1 1 0 1 1 1]))

;;
;; BCH(15,5)
;;
(def size 10)

;;
;; xor bit-mask for resulting data string
;;
(def mask [1 0 1 0 1 0 0 0 0 0 1 0 0 1 0])


(defn encode [data]
  (let [ecc-bits (utils/n-to-bits (bch/divide (utils/bits-to-n data) gp size) size)
        bits (apply conj data ecc-bits)]
    (vec (map #(bit-xor %1 %2) bits mask))))

