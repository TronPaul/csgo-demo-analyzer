(ns csgo-demo-reader.bit-buf
  (:import (csgo_demo_reader BitBuffer)
           (java.nio ByteBuffer)))

(defn bit-buffer [^ByteBuffer byte-buffer]
  (BitBuffer. byte-buffer))

(defn read-bits [^BitBuffer bit-buf n]
  (.readBits bit-buf n))

(defn read-bool [^BitBuffer bit-buf]
  (.readBool bit-buf))

(defn read-var-int-32 [^BitBuffer bit-buf]
  (.readVarInt32 bit-buf))

(defn read-var-int-64 [^BitBuffer bit-buf]
  (.readVarInt64 bit-buf))

(defn read-signed-bits [^BitBuffer bit-buf n]
  (.readSignedBits bit-buf n))

(defn read-unsigned-bit-var [^BitBuffer bit-buf]
  (.readUnsignedBitVar bit-buf))

(defn read-bit-angle [^BitBuffer bit-buf n]
  (.readBitAngle bit-buf n))

(defn read-bit-coord [^BitBuffer bit-buf]
  (.readBitCoord bit-buf))

(defn read-bit-cell-coord [^BitBuffer bit-buf n coord-type]
  (.readBitCellCoord bit-buf n coord-type))

(defn read-bit-coord-mp [^BitBuffer bit-buf coord-type]
  (.readBitCoordMP bit-buf coord-type))

(defn read-bit-normal [^BitBuffer bit-buf]
  (.readBitNormal bit-buf))

(defn read-bit-float [^BitBuffer bit-buf]
  (.readBitFloat bit-buf))

(defn read-signed-var-int-64 [^BitBuffer bit-buf]
  (.readSignedVarInt64 bit-buf))

(defn read-signed-var-int-32 [^BitBuffer bit-buf]
  (.readSignedVarInt32 bit-buf))