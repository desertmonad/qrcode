(ns qrcode.bch
  (:require [qrcode.sequence :as sequence])
  (:require [qrcode.utils :as utils]))

(defn divide
  "calculate bch error correction bits for numer using denom and bch-size error bits"
  [numer denom bch-size]
  (let [dd (utils/msb denom)]
    (loop [target (bit-shift-left numer bch-size)]
      (let [dt (utils/msb target)]
        (if (>= dt dd)
          (recur (bit-xor target (bit-shift-left denom (- dt dd))))
          target)))))
