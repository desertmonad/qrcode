(ns qrcode.galois)

(def pp 285)
(def size 255)

(def antilog
  (let [stop (dec size)]
    (loop [values [1]
           n 1
           x 1]
      (if (> n stop)
        values
        (let [next (* x 2)
              val (if (> next size) (bit-xor next pp) next)]
          (if (not (contains? (set values) val))
            (recur (conj values val) (inc n) val)))))))

(def log
  (let [cnt (count antilog)]
    (vec (map (apply hash-map (interleave antilog (range cnt))) (range (inc cnt))))))

(defn add [& more]
  (if (seq more)
    (reduce bit-xor more)
    0))

(def subtract add)


(defn- mult [a b] {:pre [(and (>= a 0) (<= a size)) (and (>= b 0) (<= b size))]}
  (if (or (= a 0) (= b 0))
    0
    (antilog (mod (+ (log a) (log b)) size))))

(defn multiply [& more] (reduce mult more))


(defn- div [a b] {:pre [(and (>= a 0) (<= a size)) (and (> b 0) (<= b size))]}
  (if (= a 0)
    0
    (antilog (mod (- (log a) (log b)) size))))

(defn divide [& more] (reduce div more))


(defn generator-polynomial [n]
  (loop [results [1 (antilog 0)]
         i 1]
    (if (>= i n)
      results
      (let [multiplier (antilog i)
            row1 (conj results 0)
            row2 (apply conj [0] (vec (map #(multiply % multiplier) results)))]
        (recur (vec (map #(add %1 %2) row1 row2)) (inc i))))))


;;
;; polynomial functions using the galois field above
;;

(defn degree [a] (count a))

(defn poly-divide* [numerator denominator]
  (let [nc (first numerator)
        size (degree numerator)]
    (loop [result []
           n 0]
      (if (= n size)
        (vec (rest result))
        (recur (conj result (add (get numerator n) (multiply nc (get denominator n 0)))) (inc n))))))

(defn poly-divide [numerator denominator]
  (let [dd (degree denominator)]
    (loop [numer numerator]
      (let [dn (degree numer)]
        (if (< dn dd)
          numer
          (recur (poly-divide* numer denominator)))))))

