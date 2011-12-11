(ns qrcode.test.encode
  (:use clojure.test)
  (:use qrcode.encode))

(deftest numeric-mode-encoding
  (is (= [0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 1 0 1 1 0 0 1 1 0 0 0 0 1 1] (numeric-mode "01234567" 1)))
  (is (= [0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 1 0 1 1 0 0 1 1 0 1 0 1 0 0 1 1 0 1 1 1 0 0 0 0 1 0 1 0 0 1 1 1 0 1 0 1 0 0 1 0 1] (numeric-mode "0123456789012345" 1))))


(deftest alphanumeric-mode-encoding
  (is (= [0 0 1 0 0 0 0 0 0 0 1 0 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 1 0 0 1 0 0 0 0 1 0] (alphanumeric-mode "AC-42" 1))))


(deftest eight-bit-byte-mode-encoding
  (is (= [0 1 0 0 0 0 0 0 0 0 1 1 0 1 1 0 0 0 0 1 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 1] (eight-bit-byte-mode "abc" 1))))

(deftest codeword-generation
  (let [bits [0 1 0 0 0 0 0 0 0 0 1 1 0 1 1 0 0 0 0 1 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 1]]
    (is (= [64 54 22 38 48] (codewords bits)))))

(deftest find-encoding-type
  (is (= {:type :numeric :f numeric-mode} (encoding-type "0123456789")))
  (is (= {:type :alphanumeric :f alphanumeric-mode} (encoding-type "HTTP://WWW.CLOCKPARTNERS.COM")))
  (is (= {:type :8bit :f eight-bit-byte-mode} (encoding-type "http://www.clockpartners.com"))))

(deftest best-version-for-payload
  (is (= 1 (find-best-version :numeric :L "01234567890"))))

(deftest encoding
  (let [payload "http://clockpartners.com"
        expected {:version 2,
                  :type :8bit,
                  :size 34,
                  :error-level :L,
                  :original "http://clockpartners.com",
                  :original-length 24,
                  :encoded  [65 134 135 71 71 3 162 242 246 54 198 246 54 183 6 23 39
                             70 230 87 39 50 230 54 246 208 236 17 236 17 236 17 236 17],
                  :encoded-length 34}]
    (is (= expected (encode-data payload :L)))))
