(ns qrcode.utils)

(defn position-value-pairs
  "create a vector of vectors that contain a position and value"
  [positions values]
  (vec (map vec (partition 2 (interleave positions values)))))

(defn combinations
  "return all combinations of a sequence of values"
  [values]
  (let [size (count values)]
    (loop [results []
           i 0]
      (if (< i size)
        (let [target (nth values i)
              combinations (for [x (range size)] [target (nth values x)])]
          (recur (apply conj results combinations) (inc i)))
        results))))

(defn- valid-number
  "check n is a byte, short, int or long"
  [n]
  (let [_class (class n)]
    (or (= _class java.lang.Byte)
        (= _class java.lang.Short)
        (= _class java.lang.Integer)
        (= _class java.lang.Long))))

(defn n-to-bits
  "convert 'valid-number' to a binary number array sequence ie. (byte 5) -> [0 0 0 0 0 1 0 1]
   IF nbits are specified (2nd form) then the returning array ONLY has nbits (rest are ignored)"
  ([n] {:pre [(valid-number n)]}
     (n-to-bits n (eval `(. ~(class n) SIZE))))
  ([n nbits] {:pre [(valid-number n)]}
      (vec (reverse (for [x (range nbits)] (bit-and 0x01 (bit-shift-right n x)))))))

(defn bits-to-n
  "convert a sequence of zeros and ones to its corresponding number ie. [0 1 0 1] -> 5"
  [bits]
  (let [size (count bits)]
    (reduce + (for [x (range size 0 -1)] (bit-shift-left (bits (- size x)) (dec x))))))

(defn msb
  "find the most significant bit set in this 'valid-number'"
  [n]
  (count (drop-while zero? (n-to-bits n))))
