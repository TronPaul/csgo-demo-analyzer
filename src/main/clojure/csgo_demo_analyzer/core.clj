(ns csgo-demo-analyzer.core
  (:require [clojure.java.io :as io]
            [clojurewerkz.buffy.core :refer :all])
  (:use [protobuf.core])
  (:import (clojurewerkz.buffy.types.protocols BuffyType)))

(deftype Int32LEType []
  BuffyType
  (size [_] 4)
  (write [bt buffer idx value]
    (.setInt buffer idx (Integer/reverseBytes value)))
  (read [by buffer idx]
    (Integer/reverseBytes (.getInt buffer idx)))

  (rewind-write [bt buffer value]
    (.writeInt buffer (Integer/reverseBytes value)))
  (rewind-read [by buffer]
    (Integer/reverseBytes (.readInt buffer)))

  Object
  (toString [_]
    "Int32LEType"))

(def int32LE-type   (memoize #(Int32LEType.)))

(def demo-header
  (spec :filestamp (string-type 8)
        :protocol (int32LE-type)
        :network-protocol (int32LE-type)
        :server-name (string-type 260)
        :client-name (string-type 260)
        :map-name (string-type 260)
        :game-directory (string-type 260)
        :playback-time (float-type)
        :playback-ticks (int32LE-type)
        :playback-frames (int32LE-type)
        :signon-length (int32LE-type)))

(defn read-demo-header [input-stream]
  (let [buf (compose-buffer demo-header)]
    (.setBytes (buffer buf) 0 input-stream 1072)
    (decompose buf)))

(defn read-demo [fname]
  (with-open [is (io/input-stream fname)]
    (read-demo-header is)))
