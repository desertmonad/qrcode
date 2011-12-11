(ns qrcode.core
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.definitions :as defs])
  (:require [qrcode.matrix :as matrix])
  (:require [qrcode.pattern :as pattern])
  (:require [qrcode.encode :as encode])
  (:require [qrcode.ecc :as ecc])
  (:require [qrcode.mask :as mask])
  (:require [qrcode.format :as format])
  (:require [qrcode.utils :as utils]))

(defn generate-base-qrcode [{:keys [version ecc-encoded] :as m}]
  (let [size (+ (* version 4) 17)
        matrix (pattern/generate-usability-pattern version)
        values (flatten (apply conj (vec (map #(utils/n-to-bits % 8) ecc-encoded)) [0 0 0 0 0 0 0]))
        pv-pairs (utils/position-value-pairs
                  (sequences/valid-bitposition-sequence (pattern/generate-usability-pattern version))
                  values)]
    (merge m {:qrcode (matrix/set-pv-pairs matrix pv-pairs)})))

(defn encode-format-information [{:keys [error-level qrcode version mask-code] :as m}]
  (let [format-data (format/encode (vec (apply conj (defs/error-level-indicator error-level) mask-code)))
        primary-format-pv-data   (utils/position-value-pairs (sequences/primary-format-sequence version) format-data)
        secondary-format-pv-data (utils/position-value-pairs (sequences/secondary-format-sequence version) format-data)]
    (merge m {:format-data format-data
              :qrcode (-> qrcode
                        (matrix/set-pv-pairs primary-format-pv-data)
                        (matrix/set-pv-pairs secondary-format-pv-data))})))

(defn encode [message error-level]
  (-> (encode/encode-data message error-level)
      (ecc/generate-ecc)
      (generate-base-qrcode)
      (mask/find-best-mask)
      (encode-format-information)))

(def *quiet-zone-size* 4)
(def *pixels-per-square* 5)

(defn draw [{:keys [qrcode]:as m}]
  (let [rows (count qrcode)
        cols (count (first qrcode))
        size (* (+ (count qrcode) (* *quiet-zone-size* 2)) *pixels-per-square*)
        image (java.awt.image.BufferedImage. size size java.awt.image.BufferedImage/TYPE_BYTE_BINARY)
        gc    (.getGraphics image)]
    (doto gc
      (.setColor java.awt.Color/WHITE)
      (.fillRect 0 0 size size)
      (.setColor java.awt.Color/BLACK))
    (doseq [row (range rows) col (range cols)]
      (let [on-off (matrix/get-row-col qrcode row col)
            offset-y (* (+ row *quiet-zone-size*) *pixels-per-square*)
            offset-x (* (+ col *quiet-zone-size*) *pixels-per-square*)]
        (if (= on-off 1)
          (doto gc
            (.fillRect offset-x offset-y *pixels-per-square* *pixels-per-square*)))))
    (merge m {:image image})))


(defn save-as [{:keys [image] :as m} file]
  (javax.imageio.ImageIO/write image "PNG" (java.io.File. (str file ".png")))
  (merge m {:file file}))
