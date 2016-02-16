(ns csgo-demo-reader.bit-buffer
  (:require [octet.core :as buf]
            [csgo-demo-reader.spec :as spec]
            [csgo-demo-reader.util :as util]))

(def bit-mask-table
  (concat [0]
          (map #(- (bit-shift-left 1 %) 1) (range 1 31))
          [0x7fffffff
           0xffffffff]))

(defn next-word [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (buf/read size-buf spec/int32LE)))

(defn next-bits [input-stream]
  {:size 32 :word (next-word input-stream)})

(defn read-bits [cur-bits num-bits]
  [(bit-and (:word cur-bits) (nth bit-mask-table num-bits))
   {:size (- (:size cur-bits) num-bits) :word (bit-shift-right (:word cur-bits) num-bits)}])

(defn seek-bits
  [input-stream cur-bits num-bits prep-next-word]
  (if (>= (:size cur-bits) num-bits)
    [num-bits (second (read-bits cur-bits num-bits))]
    (let [num-bits-rem (- num-bits (:size cur-bits))
          num-bytes (int (/ num-bits-rem 8))
          rem (mod num-bits-rem 8)]
      (util/safe-skip input-stream num-bytes)
      (if prep-next-word
        [num-bits {:size (- 32 rem) :word (bit-shift-right (next-word input-stream) rem)}]
        [num-bits nil]))))

(defn read-ubit-long [input-stream cur-bits num-bits]
  (if (>= (:size cur-bits) num-bits)
    (let [[ret cur-bits] (read-bits cur-bits num-bits)]
      (if (zero? (:size cur-bits))
        [ret (next-bits input-stream)]
        [ret cur-bits]))
    (if (= 0 (:size cur-bits))
      (read-bits (next-bits input-stream) num-bits))))

(defn read-var-int32 [input-stream bits]
  (loop [count 0
         result 0
         cur-bits bits]
    (let [[b new-bits] (read-ubit-long input-stream cur-bits 8)
          new-result (bit-or result (bit-shift-left (bit-and b 0x7f) (* 7 count)))
          count (inc count)]
      (if (or (= count 5) (zero? (bit-and b 0x80)))
        [(* count 8) new-result new-bits]
        (recur count new-result new-bits)))))
