(ns csgo-demo-reader.core
  (:require [clojure.java.io :as io]
            [octet.core :as buf]
            [octet.spec :as spec]
            [octet.spec.basic :as basic]
            [octet.buffer :as buffer])
  (:use [protobuf.core])
  (:import (com.valve NetmessagesPublic)))

(defn read-int32LE [_ pos]
  (Integer/reverseBytes (buffer/read-int _ pos)))

(defn write-int32LE [_ pos value]
  (buffer/write-int _ pos (Integer/reverseBytes value)))

(def int32LE
  (basic/primitive-spec 4 read-int32LE write-int32LE))

(def bit-mask-table
  (concat [0]
          (map #(- (bit-shift-left 1 %) 1) (range 1 31))
          [0x7fffffff
           0xffffffff]))

(defn next-word [input-stream]
  (let [size-buf (buf/allocate (buf/size int32LE))]
    (.read input-stream (.array size-buf))
    (buf/read size-buf int32LE)))

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
          new-result (bit-or result (bit-shift-left (bit-and b 0x7f) (* 7 count)))]
      (if (or (= count 5) (bit-and b 0x80))
        new-result
        (recur (inc count) new-result new-bits)))))

(defrecord Vector [x y z])

(defn read-many [value-types buff pos]
  (reduce (fn [coll value-type]
            (let [prev-readed (first coll)
                  cur-pos (+ pos (first coll))
                  [readed ret] (spec/read value-type buff cur-pos)]
              [(+ prev-readed readed) (conj (second coll) ret)]))
          [0 []] value-types))

(def vector-spec
  (reify
    spec/ISpecSize
    (size [_]
      (* 3 (spec/size buf/float)))

    spec/ISpec
    (read [_ buff pos]
      (let [[readed vals] (read-many (repeat 3 (buf/float)) buff pos)]
        [readed (apply ->Vector vals)]))

    (write [_ buff pos vector])))

(defrecord Split [flags
                  view-origin
                  view-angle
                  local-view-angle
                  resample-view-origin
                  resample-view-angle
                  resample-local-view-angle])

(def split-spec
  (reify
    spec/ISpecSize
    (size [_]
      (+ (spec/size buf/int32) (* 6 (spec/size vector-spec))))

    spec/ISpec
    (read [_ buff pos]
      (let [[readed vals] (read-many (cons buf/int32 (repeat 6 vector-spec)) buff pos)]
        [readed (apply ->Split vals)]))
    (write [_ buff pos vector])))

(def demo-header
  (buf/spec :filestamp  (buf/string 8)
        :protocol int32LE
        :network-protocol int32LE
        :server-name (buf/string 260)
        :client-name (buf/string 260)
        :map-name (buf/string 260)
        :game-directory (buf/string 260)
        :playback-time buf/float
        :playback-ticks int32LE
        :playback-frames int32LE
        :signon-length int32LE))

(def cmd-header
  (buf/spec :cmd buf/byte
            :tick int32LE
            :player_slot buf/byte))

(def demo-cmd-info
  (buf/spec :p1 split-spec
            :p2 split-spec))

(def sequence-info
  (buf/spec :seq-in int32LE
            :seq-out int32LE))

(defn skip-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (long (buf/read size-buf int32LE))]
      (loop [pos 0]
        (if (<= size pos)
          pos
          (recur (+ pos (.skip input-stream (- size pos)))))))))

(defn read-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (buf/read size-buf int32LE)
          b (byte-array size)]
      (loop [pos 0]
        (if (<= size pos)
          pos
          (recur (+ pos (.read input-stream b pos (- size pos)))))))))

(defn read-demo-header [input-stream]
  (let [b (buf/allocate (buf/size demo-header))]
    (.read input-stream (.array b))
    (buf/read b demo-header)))

(defn read-cmd-header [input-stream]
  (let [b (buf/allocate (buf/size cmd-header))]
    (.read input-stream (.array b))
    (buf/read b cmd-header)))

(defn read-demo-cmd-info [input-stream]
  (let [b (buf/allocate (buf/size demo-cmd-info))]
    (.read input-stream (.array b))
    (buf/read b demo-cmd-info)))

(defn read-sequence-info [input-stream]
  (let [b (buf/allocate (buf/size sequence-info))]
    (.read input-stream (.array b))
    (buf/read b sequence-info)))

(defn read-packet-size [input-stream]
  (let [size-buf (buf/allocate (buf/size int32LE))]
    (.read input-stream (.array size-buf))
    (buf/read size-buf int32LE)))

(defn read-demo-packet [input-stream]
  (let [packet-size (read-packet-size input-stream)]
    (println "packet-size: " packet-size)
    (loop [rel-pos 0
           cur-bits {:size 32 :word (next-word input-stream)}]
      (println cur-bits)
      (if (<= packet-size rel-pos)
        (do
          (println rel-pos)
          rel-pos)
        (let [cmd (read-var-int32 input-stream cur-bits)
              size (read-var-int32 input-stream cur-bits)]
          (println cmd)
          (println size)
          (throw (RuntimeException.))
          (recur 0 0))))))

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