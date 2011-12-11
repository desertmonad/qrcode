(ns qrcode.encode
  (:require [qrcode.sequence :as sequences])
  (:require [qrcode.definitions :as defs])
  (:require [qrcode.utils :as utils]))

;;
;; count-bits --- determine how many bits make up the count prepended
;; to the encoded data
;;

(defn count-bits-dispatch-fn [mode _] mode)

(defmulti count-bits count-bits-dispatch-fn)

(defmethod count-bits :numeric [_ version]
  (cond
   (>= version 27) 14
   (>= version 10) 12
   :default 10))

(defmethod count-bits :alphanumeric [_ version]
  (cond
   (>= version 27) 13
   (>= version 10) 11
   :default 9))

(defmethod count-bits :8bit [_ version]
  (cond
   (>= version 10) 16
   :default 8))

;;
;; numeric mode encoding
;;

(defn encode-numeric [s]
  (let [len (count s)
        bits (cond (= len 3) 10 (= len 2) 7 (= len 1) 4)
        n (Integer/parseInt s)]
    (utils/n-to-bits n bits)))

(defn numeric-mode [s version]
  (let [indicator [0 0 0 1]
        c (count s)
        bits (->> (map #(apply str %) (partition 3 3 nil s))
                  (map encode-numeric)
                  (vec))]
    (vec (flatten [indicator (utils/n-to-bits c (count-bits :numeric version)) bits]))))

;;
;; alphanumeric mode encoding
;;

(def alphanumeric-mapping
  {\0  0 \1  1 \2  2 \3  3 \4  4 \5  5 \6  6 \7  7 \8  8 \9  9
   \A 10 \B 11 \C 12 \D 13 \E 14 \F 15 \G 16 \H 17 \I 18 \J 19
   \K 20 \L 21 \M 22 \N 23 \O 24 \P 25 \Q 26 \R 27 \S 28 \T 29
   \U 30 \V 31 \W 32 \X 33 \Y 34 \Z 35 \  36 \$ 37 \% 38 \* 39
   \+ 40 \- 41 \. 42 \/ 43 \: 44})

(defn encode-alphanumeric [[x1 x2]]
  (let [pair? (if (not (nil? x2)) true false)
        val (if pair? (+ (* x1 45) x2) x1)
        bits (if pair? 11 6)]
    (utils/n-to-bits val bits)))

(defn alphanumeric-mode [s version]
  (let [indicator [0 0 1 0]
        c (count s)
        bits (->> (map alphanumeric-mapping s)
                  (partition 2 2 nil)
                  (map vec)
                  (map encode-alphanumeric))]
    (vec (flatten [indicator (utils/n-to-bits c (count-bits :alphanumeric version)) bits]))))


;;
;; 8bit mode encoding
;;

(defn eight-bit-byte-mode [s version]
  (let [indicator [0 1 0 0]
        c (count s)
        bits (->> (map #(int %) s)
                  (map #(utils/n-to-bits % 8))
                  (map vec))]
    (vec (flatten [indicator (utils/n-to-bits c (count-bits :8bit version)) bits]))))

;;
;; generate codewords from bit strings
;;

(defn codewords [encoding]
  (let [terminator [0 0 0 0]
        bits (apply conj encoding terminator)]
    (vec (map  utils/bits-to-n (map vec (partition 8 8 sequences/zeros bits))))))

;;
;; encode data string based on content
;;

(defn isnumeric? [s] (re-matches #"\d+" s))

(defn isalphanumeric? [s] (re-matches #"[\dA-Z $%*+-\./:]+" s))

(defn is8bit? [s] (every? #(and (>= (int %) 0x00) (<= (int %) 0x7F)) s))

(defn find-best-version [type error-level s]
  (let [sizes ((defs/data-capacities type) error-level)
        size (count s)
        version (inc (count (filter #(< % size) sizes)))]
    (if (> version (count sizes))
      (throw (Exception. "message too large for type and error-level! Unable to encode"))
      version)))

(defn encoding-type [s]
  (cond
   (isnumeric? s)      {:type :numeric :f numeric-mode}
   (isalphanumeric? s) {:type :alphanumeric :f alphanumeric-mode}
   (is8bit? s)         {:type :8bit :f eight-bit-byte-mode}
   :default            (throw (Exception. "invalid message! Unable to encode"))))

(defn encode-data [s error-level]
  (if (> (count s) 0)
    (let [{:keys [f type]} (encoding-type s)
          version (find-best-version type error-level s)
          payload (codewords (f s version))
          size ((defs/codeword-capacities error-level) (dec version))
          padded (vec (flatten (merge payload (take (- size (count payload)) sequences/padding-codewords))))]
      {:version version
       :type type
       :size size
       :error-level error-level
       :original s
       :original-length (count s)
       :encoded padded
       :encoded-length (count padded)})
    (throw (Exception. (format "no message found to encode! shamefully giving up: message=[%s]" s)))))
