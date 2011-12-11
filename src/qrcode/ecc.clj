(ns qrcode.ecc
  (:require [qrcode.galois :as galois])
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.definitions :as capacities]))

(defn merge-blocks* [blocks]
  (loop [blocks (filter #(not (nil? (seq %))) blocks)
         results []]
    (if (not (> (count blocks) 0))
      results
      (let [data (map #(first %) blocks)
            next-set (map #(rest %) blocks)]
        (recur
         (filter #(not (nil? (seq %))) next-set)
         (vec (flatten (apply conj results data))))))))

(defn merge-blocks [blocks]
  (let [data-blocks (map #(first %) blocks)
        ecc-blocks (map #(second %) blocks)]
    (vec (flatten (merge (merge-blocks* data-blocks) (merge-blocks* ecc-blocks))))))

(defn generate-ecc [{:keys [encoded error-level version] :as m}]
  (let [capacities (capacities/error-correction-characteristics (dec version))
        ecc-characteristics (capacities error-level)
        [_ [tc dc]] ecc-characteristics
        gp-size (- tc dc)
        zero-padding (take gp-size sequences/zeros)
        denominator (galois/generator-polynomial gp-size)]
    (loop [results []
           data encoded
           ecc-char ecc-characteristics]
      (if (< (count ecc-char) 2)
        (let [final-results (merge-blocks results)]
          (merge m {:ecc-encoded final-results :ecc-encoded-length (count final-results)}))
        (let [ecc-spec (vec (take 2 ecc-char))
              [n [tc dc]] ecc-spec
              divisions (for [x (range n)]
                          (let [numerator (vec (take dc (drop (* x dc) data)))]
                            [numerator (galois/poly-divide (vec (apply conj numerator zero-padding)) denominator)]))]
          (recur
           (apply conj results divisions)
           (drop (* n dc) data)
           (drop 2 ecc-char)))))))

