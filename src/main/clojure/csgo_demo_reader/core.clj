(ns csgo-demo-reader.core
  (:require [clojure.java.io :as io]
            [octet.core :as buf]
            [csgo-demo-reader.spec :as spec])
  (:use [protobuf.core])
  (:import (com.valve NetmessagesPublic)))

(def bit-mask-table
  (concat [0]
          (map #(- (bit-shift-left 1 %) 1) (range 1 31))
          [0x7fffffff
           0xffffffff]))

(defn safe-skip [input-stream num-bytes]
  (loop [pos 0]
    (if (<= num-bytes pos)
      pos
      (recur (+ pos (.skip input-stream (- num-bytes pos)))))))

(defn next-word [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (buf/read size-buf spec/int32LE)))

(defn seek-bits [input-stream cur-bits num-bits]
  (if (>= (:size cur-bits) num-bits)
    [num-bits {:size (- (:size cur-bits) num-bits) :word (bit-shift-right (:word cur-bits) num-bits)}]
    (let [num-bits-rem (- num-bits (:size cur-bits))
          num-bytes (int (/ num-bits-rem 8))
          rem (mod num-bits-rem 8)]
      (safe-skip input-stream num-bytes)
      [num-bits {:size (- 32 rem) :word (bit-shift-right (next-word input-stream) rem)}])))

(defn read-ubit-long [input-stream cur-bits num-bits]
  (if (>= (:size cur-bits) num-bits)
    (let [ret (bit-and (:word cur-bits) (nth bit-mask-table num-bits))
          new-size (- (:size cur-bits) num-bits)]
      (if (not (zero? new-size))
        [ret {:size new-size :word (bit-shift-right (:word cur-bits) num-bits)}]
        [ret {:size 32 :word (next-word input-stream)}]))))

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

(defn skip-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (long (buf/read size-buf spec/int32LE))]
      (safe-skip input-stream size))))

(defn read-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (buf/read size-buf spec/int32LE)
          b (byte-array size)]
      (loop [pos 0]
        (if (<= size pos)
          pos
          (recur (+ pos (.read input-stream b pos (- size pos)))))))))

(defn read-demo-header [input-stream]
  (let [b (buf/allocate (buf/size spec/demo-header))]
    (.read input-stream (.array b))
    (buf/read b spec/demo-header)))

(defn read-cmd-header [input-stream]
  (let [b (buf/allocate (buf/size spec/cmd-header))]
    (.read input-stream (.array b))
    (buf/read b spec/cmd-header)))

(defn read-demo-cmd-info [input-stream]
  (let [b (buf/allocate (buf/size spec/demo-cmd-info))]
    (.read input-stream (.array b))
    (buf/read b spec/demo-cmd-info)))

(defn read-sequence-info [input-stream]
  (let [b (buf/allocate (buf/size spec/sequence-info))]
    (.read input-stream (.array b))
    (buf/read b spec/sequence-info)))

(defn read-packet-size [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (buf/read size-buf spec/int32LE)))

(defn ret-inc-pos [pos v]
  (apply conj [(+ pos (first v))] (rest v)))

(defn read-demo-packet [input-stream]
  (let [packet-size (read-packet-size input-stream)]
    (loop [rel-pos 0
           cur-bits {:size 32 :word (next-word input-stream)}]
      (if (<= packet-size (/ rel-pos 8))
        (/ rel-pos 8)
        (let [[rel-pos cmd cur-bits] (ret-inc-pos rel-pos (read-var-int32 input-stream cur-bits))
              [rel-pos size cur-bits] (ret-inc-pos rel-pos (read-var-int32 input-stream cur-bits))]
          (let [[rel-pos cur-bits] (ret-inc-pos rel-pos (seek-bits input-stream cur-bits (* 8 size)))]
            (recur rel-pos cur-bits)))))))

(defn read-data-tables [input-stream]
  (read-raw-data input-stream))

(defn read-string-tables [input-stream]
  (read-raw-data input-stream))

(defn handle-demo-packet [input-stream]
  (read-demo-cmd-info input-stream)
  (read-sequence-info input-stream)
  (read-demo-packet input-stream)
  (throw (RuntimeException.)))

(defn read-demo-cmds [input-stream]
  (let [demo-stop (atom false)]
    (while (not @demo-stop)
      (let [cmd-header (read-cmd-header input-stream)]
        (cond
          (or (= (:cmd cmd-header) 1) (= (:cmd cmd-header) 2)) (handle-demo-packet input-stream)
          (= (:cmd cmd-header) 3) nil
          (= (:cmd cmd-header) 4) (skip-raw-data input-stream)
          (= (:cmd cmd-header) 5) (throw (UnsupportedOperationException. "usercmd"))
          (= (:cmd cmd-header) 6) (read-data-tables input-stream)
          (= (:cmd cmd-header) 7) (swap! demo-stop (fn [_ v] v) true)
          (= (:cmd cmd-header) 8) (throw (UnsupportedOperationException. "customdata"))
          (= (:cmd cmd-header) 9) (read-string-tables input-stream)
          :else (println "unknown"))))))

(defn read-demo [fname]
  (with-open [is (io/input-stream fname)]
    (read-demo-header is)
    (read-demo-cmds is)))
