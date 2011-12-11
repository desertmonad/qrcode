(ns qrcode.version
  (:require [qrcode.bch :as bch])
  (:require [qrcode.utils :as utils]))

;;
;; G(x) = x^12 + x^11 + x^10 + x^9 + x^8 +x^5 + x^2 + 1
;;
(def gp (utils/bits-to-n [1 1 1 1 1 0 0 1 0 0 1 0 1]))

;;
;; BCH(18,6)
;;
(def size 12)

(defn encode [version] {:pre [(and (>= version 7) (<= version 40))]}
  (let [data (vec (utils/n-to-bits version 6))
        ecc-bits (utils/n-to-bits (bch/divide version gp size) size)]
    (apply conj data ecc-bits)))

