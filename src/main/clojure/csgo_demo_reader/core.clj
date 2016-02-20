(ns csgo-demo-reader.core
  (:require [clojure.java.io :as io]
            [octet.core :as buf]
            [csgo-demo-reader.spec :as spec]
            [csgo-demo-reader.util :as util])
  (:use [flatland.protobuf.core])
  (:import (com.valve NetmessagesPublic)))

(def unknown-msg-count (atom 0))

(defn skip-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (long (buf/read size-buf spec/int32LE))]
      (util/safe-skip input-stream size))))

(defn read-raw-data [input-stream]
  (let [size-buf (buf/allocate (buf/size spec/int32LE))]
    (.read input-stream (.array size-buf))
    (let [size (buf/read size-buf spec/int32LE)
          b (byte-array size)]
      (util/safe-read input-stream b))))

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
    (loop [rel-pos 0]
      (if (<= packet-size rel-pos)
        rel-pos
        (let [[rel-pos cmd] (ret-inc-pos rel-pos (spec/read-var-int32 input-stream))
              [rel-pos size] (ret-inc-pos rel-pos (spec/read-var-int32 input-stream))]
          (if (< 31 cmd)
            (swap! unknown-msg-count inc))
          (let [rel-pos (+ rel-pos (util/safe-skip input-stream size))]
            (recur rel-pos)))))))

(defn read-data-tables [input-stream]
  (read-raw-data input-stream))

(defn read-string-tables [input-stream]
  (read-raw-data input-stream))

(defn handle-demo-packet [input-stream]
  (read-demo-cmd-info input-stream)
  (read-sequence-info input-stream)
  (read-demo-packet input-stream))

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
