(ns csgo-demo-reader.spec
  (:require [octet.core :as buf]
            [octet.buffer :as buffer]
            [octet.spec.basic :as basic]
            [octet.spec :as spec]))

(defn read-int32LE [_ pos]
  (Integer/reverseBytes (buffer/read-int _ pos)))

(defn write-int32LE [_ pos value]
  (buffer/write-int _ pos (Integer/reverseBytes value)))

(def int32LE
  (basic/primitive-spec 4 read-int32LE write-int32LE))

(defn read-var-int32 [input-stream]
  (loop [count 0
         result 0]
    (let [b (.read input-stream)
          new-result (bit-or result (bit-shift-left (bit-and b 0x7f) (* 7 count)))
          num-bytes-read (inc count)]
      (if (or (= num-bytes-read 5) (zero? (bit-and b 0x80)))
        [num-bytes-read new-result]
        (recur num-bytes-read new-result)))))

(defn read-short [input-stream]
  (let [b (.read input-stream)]
    (bit-or (bit-shift-left (.read input-stream) 8) b)))

(defn read-string [input-stream size]
  (loop [count 0
         acc []]
    (if (< count size)
      (let [c-code (.read input-stream)]
        (if (zero? c-code)
          acc
          (recur (inc count) (conj acc (char c-code)))))
      acc)))

(defn read-many [value-types buff pos]
  (reduce (fn [coll value-type]
            (let [prev-readed (first coll)
                  cur-pos (+ pos (first coll))
                  [readed ret] (spec/read value-type buff cur-pos)]
              [(+ prev-readed readed) (conj (second coll) ret)]))
          [0 []] value-types))

(defrecord Vector [x y z])

(def cs-vector
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

(def split
  (reify
    spec/ISpecSize
    (size [_]
      (+ (spec/size buf/int32) (* 6 (spec/size cs-vector))))

    spec/ISpec
    (read [_ buff pos]
      (let [[readed vals] (read-many (cons buf/int32 (repeat 6 cs-vector)) buff pos)]
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
  (buf/spec :p1 split
            :p2 split))

(def sequence-info
  (buf/spec :seq-in int32LE
            :seq-out int32LE))