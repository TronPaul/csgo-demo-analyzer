(ns csgo-demo-reader.core
  (:require [clojure.java.io :as io]
            [octet.core :as buf]
            [csgo-demo-reader.spec :as spec]
            [csgo-demo-reader.util :as util]
            [csgo-demo-reader.bit-buf :as bit-buf])
  (:use [flatland.protobuf.core])
  (:import (com.valve NetmessagesPublic$CNETMsg_NOP NetmessagesPublic$CNETMsg_Disconnect NetmessagesPublic$CNETMsg_File
                      NetmessagesPublic$CNETMsg_Tick NetmessagesPublic$CNETMsg_StringCmd NetmessagesPublic$CNETMsg_SetConVar
                      NetmessagesPublic$CNETMsg_SignonState NetmessagesPublic$CSVCMsg_ServerInfo NetmessagesPublic$CSVCMsg_SendTable
                      NetmessagesPublic$CSVCMsg_ClassInfo NetmessagesPublic$CSVCMsg_SetPause NetmessagesPublic$CSVCMsg_CreateStringTable
                      NetmessagesPublic$CSVCMsg_UpdateStringTable NetmessagesPublic$CSVCMsg_VoiceInit NetmessagesPublic$CSVCMsg_VoiceData
                      NetmessagesPublic$CSVCMsg_Print NetmessagesPublic$CSVCMsg_Sounds NetmessagesPublic$CSVCMsg_SetView
                      NetmessagesPublic$CSVCMsg_FixAngle NetmessagesPublic$CSVCMsg_CrosshairAngle NetmessagesPublic$CSVCMsg_BSPDecal
                      NetmessagesPublic$CSVCMsg_UserMessage NetmessagesPublic$CSVCMsg_GameEvent NetmessagesPublic$CSVCMsg_PacketEntities
                      NetmessagesPublic$CSVCMsg_TempEntities NetmessagesPublic$CSVCMsg_Prefetch NetmessagesPublic$CSVCMsg_Menu
                      NetmessagesPublic$CSVCMsg_GameEventList NetmessagesPublic$CSVCMsg_GetCvarValue)))

(def unknown-msg-count (atom 0))

(def NOP (protodef NetmessagesPublic$CNETMsg_NOP))
(def Disconnect (protodef NetmessagesPublic$CNETMsg_Disconnect))
(def File (protodef NetmessagesPublic$CNETMsg_File))
(def Tick (protodef NetmessagesPublic$CNETMsg_Tick))
(def StringCmd (protodef NetmessagesPublic$CNETMsg_StringCmd))
(def SetConVar (protodef NetmessagesPublic$CNETMsg_SetConVar))
(def SignonState (protodef NetmessagesPublic$CNETMsg_SignonState))
(def ServerInfo (protodef NetmessagesPublic$CSVCMsg_ServerInfo))
(def SendTable (protodef NetmessagesPublic$CSVCMsg_SendTable))
(def ClassInfo (protodef NetmessagesPublic$CSVCMsg_ClassInfo))
(def SetPause (protodef NetmessagesPublic$CSVCMsg_SetPause))
(def CreateStringTable (protodef NetmessagesPublic$CSVCMsg_CreateStringTable))
(def UpdateStringTable (protodef NetmessagesPublic$CSVCMsg_UpdateStringTable))
(def VoiceInit (protodef NetmessagesPublic$CSVCMsg_VoiceInit))
(def VoiceData (protodef NetmessagesPublic$CSVCMsg_VoiceData))
(def Print (protodef NetmessagesPublic$CSVCMsg_Print))
(def Sounds (protodef NetmessagesPublic$CSVCMsg_Sounds))
(def SetView (protodef NetmessagesPublic$CSVCMsg_SetView))
(def FixAngle (protodef NetmessagesPublic$CSVCMsg_FixAngle))
(def CrosshairAngle (protodef NetmessagesPublic$CSVCMsg_CrosshairAngle))
(def BSPDecal (protodef NetmessagesPublic$CSVCMsg_BSPDecal))
(def UserMessage (protodef NetmessagesPublic$CSVCMsg_UserMessage))
(def GameEvent (protodef NetmessagesPublic$CSVCMsg_GameEvent))
(def PacketEntities (protodef NetmessagesPublic$CSVCMsg_PacketEntities))
(def TempEntities (protodef NetmessagesPublic$CSVCMsg_TempEntities))
(def Prefetch (protodef NetmessagesPublic$CSVCMsg_Prefetch))
(def Menu (protodef NetmessagesPublic$CSVCMsg_Menu))
(def GameEventList (protodef NetmessagesPublic$CSVCMsg_GameEventList))
(def GetCvarValue (protodef NetmessagesPublic$CSVCMsg_GetCvarValue))

(def commands
  {0 NOP
   1 Disconnect
   2 File
   4 Tick
   5 StringCmd
   6 SetConVar
   7 SignonState
   8 ServerInfo
   9 SendTable
   10 ClassInfo
   11 SetPause
   12 CreateStringTable
   13 UpdateStringTable
   14 VoiceInit
   15 VoiceData
   16 Print
   17 Sounds
   18 SetView
   19 FixAngle
   20 CrosshairAngle
   21 BSPDecal
   23 UserMessage
   25 GameEvent
   26 PacketEntities
   27 TempEntities
   28 Prefetch
   29 Menu
   30 GameEventList
   31 GetCvarValue})

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

(defn read-net-message-header [input-stream]
  (reduce (fn [[bytes-read-acc header] name]
            (let [[bytes-read ret] (spec/read-var-int32 input-stream)]
              [(+ bytes-read-acc bytes-read) (assoc header name ret)])) [0 {}] [:cmd :size]))

(defn read-net-message [input-stream demo-data handler-fns]
  (let [[num-bytes-read {:keys [cmd size]}] (read-net-message-header input-stream)
        cmd-proto (get commands cmd)]
    (if cmd-proto
      (let [b (buf/allocate size)]
        (util/safe-read input-stream (.array b))
        (let [msg (protobuf-load cmd-proto (.array b))]
          (if-let [cmd-fn (get (:packet-cmds handler-fns) cmd)]
            (cmd-fn msg demo-data handler-fns))
          [(+ num-bytes-read size) msg]))
      (do
        (swap! unknown-msg-count inc)
        [(+ num-bytes-read (util/safe-skip input-stream size)) nil]))))

(defn read-demo-packet [input-stream demo-data handler-fns]
  (let [packet-size (read-packet-size input-stream)]
    (loop [num-bytes-read 0
           net-messages []]
      (if (<= packet-size num-bytes-read)
        [num-bytes-read net-messages]
        (let [[new-num-bytes-read msg] (read-net-message input-stream demo-data handler-fns)]
          (recur (+ num-bytes-read new-num-bytes-read) (conj net-messages msg)))))))

(defn read-data-tables-packets [input-stream]
  (let [packet-size (read-packet-size input-stream)]
    (loop [num-bytes-read 0
           net-messages []]
      (if (<= packet-size num-bytes-read)
        [num-bytes-read net-messages]
        (let [[num-bytes-read msg] (update-in (read-net-message input-stream {} {}) [0] + num-bytes-read)]
          (if (:is-end msg)
            (do
              [num-bytes-read net-messages])
            (recur num-bytes-read (conj net-messages msg))))))))

(defn find-data-table-by-name [name data-tables]
  (first (filter #(= name (:net-table-name %)) data-tables)))

(defn parse-properties
  ([data-table data-tables-by-name]
    (parse-properties data-table data-tables-by-name []))
  ([data-table data-tables-by-name path]
   (loop [props-acc []
          props-prepend-acc []
          props-rem (:props data-table)]
     (if (not (empty? props-rem))
       (let [{:keys [type var-name flags] :as prop} (first props-rem)
             prop (assoc prop :exclude (bit-and flags (bit-shift-left 1 6)))
             props-rem (subvec props-rem 1)]
         (cond
           (= type 6) (let [prepend? (not (zero? (bit-and flags (bit-shift-left 1 11))))
                            path (if (not (= var-name "baseclass"))
                                   (conj path var-name)
                                   path)
                            new-props (parse-properties (get data-tables-by-name (:dt-name prop)) data-tables-by-name path)]
                        (if prepend?
                          (recur props-acc (into [] (concat props-prepend-acc new-props)) props-rem)
                          (recur (into [] (concat props-acc new-props)) props-prepend-acc props-rem)))
           (= type 5) (recur (conj props-acc (merge prop {:array-element (peek props-acc) :path path})) props-prepend-acc props-rem)
           :else (recur (conj props-acc (assoc prop :path path)) props-prepend-acc props-rem)))
       (into [] (concat props-prepend-acc props-acc))))))

(defn read-data-tables [input-stream demo-data handler-fns]
  (let [data-tables (second (read-data-tables-packets input-stream))
        data-tables-by-name (reduce #(assoc %1 (:net-table-name %2) %2) {} data-tables)
        num-server-classes (spec/read-short input-stream)]
    (loop [classes-read 0
           classes []]
      (if (> num-server-classes classes-read)
        (let [class-id (spec/read-short input-stream)
              name (spec/read-string input-stream 256)
              data-table-name (spec/read-string input-stream 256)
              properties (parse-properties (get data-tables-by-name data-table-name) data-tables-by-name)]
          (recur (inc classes-read) (conj classes {:class-id class-id
                                                   :name name
                                                   :data-table-name data-table-name
                                                   :properties (sort (fn [x y]
                                                                       (or (< (:priority x) (:priority y))
                                                                           (zero? (bit-and (:flags y) (bit-shift-left 1 18))))) properties)})))
        (update-in (update-in demo-data [:data-tables] (comp (partial into []) concat) data-tables) [:classes] (comp (partial into []) concat) classes)))))

(defn read-string-tables [input-stream demo-data]
  (skip-raw-data input-stream)
  demo-data)

(defn read-console-cmd [input-stream demo-data]
  (skip-raw-data input-stream)
  demo-data)

(defn read-user-cmd [input-stream demo-data]
  (skip-raw-data input-stream)
  demo-data)

(defn handle-demo-packet [input-stream demo-data handler-fns]
  (read-demo-cmd-info input-stream)
  (read-sequence-info input-stream)
  (read-demo-packet input-stream demo-data handler-fns)
  demo-data)

(defn read-demo-cmds [input-stream handler-fns]
  (loop [demo-data {}]
    (let [cmd-header (read-cmd-header input-stream)]
      (cond
        (or (= (:cmd cmd-header) 1) (= (:cmd cmd-header) 2)) (recur (handle-demo-packet input-stream demo-data handler-fns))
        (= (:cmd cmd-header) 3) (recur demo-data)
        (= (:cmd cmd-header) 4) (recur (read-console-cmd input-stream demo-data))
        (= (:cmd cmd-header) 5) (recur (read-user-cmd input-stream demo-data))
        (= (:cmd cmd-header) 6) (recur (read-data-tables input-stream demo-data handler-fns))
        (= (:cmd cmd-header) 7) nil
        (= (:cmd cmd-header) 8) (throw (UnsupportedOperationException. "Cannot parse customdata"))
        (= (:cmd cmd-header) 9) (recur (read-string-tables input-stream demo-data))
        :else (throw (UnsupportedOperationException. (str "Unknown command: " (:cmd cmd-header))))))))

(def event-types
  {1 :val-string
   2 :val-float
   3 :val-long
   4 :val-short
   5 :val-byte
   6 :val-bool
   7 :val-wstring})

; {eventid {:eventid 0 :name name :keys [{:type 1 :name name} ...]}}
(defn parse-game-event-list [game-event-list-cmd]
  (reduce #(assoc %1 (:eventid %2) %2) {} (:descriptors game-event-list-cmd)))

;{:eventid 0 :keys [{:type 1 :val-string "stuff"}...]} -> {:eventid 0 :name name :data {name "stuff" ...}}
(defn parse-game-event [game-event-cmd game-event-list]
  (if-let [game-event-info (get game-event-list (:eventid game-event-cmd))]
    (let [fields (map vector (:keys game-event-info) (:keys game-event-cmd))
          base-game-event {:eventid (:eventid game-event-cmd) :name (:name game-event-info) :data {}}]
      (reduce (fn [game-event [{:keys [type name]} val]]
                (assoc-in game-event [:data name] (get val (get event-types type)))) base-game-event fields))
    (throw (RuntimeException. "Unknown event " (:eventid game-event-cmd)))))

(defn handle-game-event [game-event-cmd game-event-list {game-event-fns :game-events :or {game-event-fns {}} :as handler-fns}]
  (let [game-event (parse-game-event game-event-cmd game-event-list)]
    (if-let [game-event-fn (get game-event-fns (:name game-event))]
      (game-event-fn game-event))))

(defn create-game-event-list-handler [game-event-list-atom]
  (fn [game-event-list-cmd handler-fns]
    (swap! game-event-list-atom merge (parse-game-event-list game-event-list-cmd))))

(defn create-game-event-handler [game-event-list-atom]
  (fn [game-event-cmd handler-fns]
    (handle-game-event game-event-cmd @game-event-list-atom handler-fns)))

(defn get-update-type [byte-buffer]
  (let [leave-pvs (bit-buf/read-bool byte-buffer)]
    (if (not leave-pvs)
      (let [enter-pvs (bit-buf/read-bool byte-buffer)]
        (if enter-pvs
          :enter
          :delta))
      (let [delete-pvs (bit-buf/read-bool byte-buffer)]
        (if delete-pvs
          :leave
          :leave)))))

(defn int-log2 [n]
  (loop [ret 0
         check n]
    (let [check (bit-shift-right check 1)]
      (if (not (zero? check))
        (recur (inc ret) check)
        (inc ret)))))

(defn read-field-index [new-way last-index byte-buffer]
  (if (and new-way (bit-buf/read-bool byte-buffer))
    (inc last-index)
    (let [ret (if (and new-way (bit-buf/read-bool byte-buffer))
                (bit-buf/read-bits byte-buffer 3)
                (let [ret (bit-buf/read-bits byte-buffer 7)
                      n-bits (get {32 2 64 4 96 7} (bit-and ret (bit-or 32 64)))]
                  (if n-bits
                    (bit-or (bit-and ret (bit-not 96)) (bit-shift-left (bit-buf/read-bits byte-buffer n-bits) 5))
                    ret)))]
      (if (= ret 0xfff)
        -1
        (+ last-index 1 ret)))))

(defn read-field-indicies [new-way byte-buffer]
  (loop [index (read-field-index new-way -1 byte-buffer)
         acc []]
    (if (= -1 index)
      acc
      (recur (read-field-index new-way index byte-buffer) (conj acc index)))))

(defn decode-int [{:keys [flags num-bits]} byte-buffer]
  (if (bit-and flags (bit-shift-left 1 19))
    (if (bit-and flags (bit-shift-left 1 0))
      (bit-buf/read-var-int-32 byte-buffer)
      (bit-buf/read-signed-var-int-32 byte-buffer))
    (if (bit-and flags (bit-shift-left 1 0))
      (bit-buf/read-bits byte-buffer num-bits)
      (bit-buf/read-signed-bits byte-buffer num-bits))))

(defn decode-float [{:keys [flags num-bits]} byte-buffer]
  (cond
    (bit-and flags (bit-shift-left 1 1)) (bit-buf/read-bit-coord byte-buffer)
    (bit-and flags (bit-shift-left 1 12)) (bit-buf/read-bit-coord-mp byte-buffer)
    (bit-and flags (bit-shift-left 1 13)) (bit-buf/read-bit-coord-mp byte-buffer :low-precision)
    (bit-and flags (bit-shift-left 1 14)) (bit-buf/read-bit-coord-mp byte-buffer :integral)
    (bit-and flags (bit-shift-left 1 2)) (bit-buf/read-bit-float byte-buffer)
    (bit-and flags (bit-shift-left 1 5)) (bit-buf/read-bit-normal byte-buffer)
    (bit-and flags (bit-shift-left 1 15)) (bit-buf/read-bit-cell-coord byte-buffer num-bits)
    (bit-and flags (bit-shift-left 1 16)) (bit-buf/read-bit-cell-coord byte-buffer num-bits :low-precision)
    (bit-and flags (bit-shift-left 1 17)) (bit-buf/read-bit-cell-coord byte-buffer num-bits :integral)
    :else (throw (RuntimeException.))))

(defn decode-vector-xy [{:keys [flags num-bits] :as property} byte-buffer]
  (let [x (decode-float property byte-buffer)
        y (decode-float property byte-buffer)]
    [x y]))

(defn decode-vector [{:keys [flags num-bits] :as property} byte-buffer]
  (let [[x y] (decode-vector-xy property byte-buffer)]
    (if (bit-and flags (bit-shift-left 1 5))
      (let [z (decode-float property byte-buffer)]
        [x y z])
      (let [negative? (bit-buf/read-bool byte-buffer)
            v0v0v1v1 (+ (* x x) (* y y))
            z (if (< v0v0v1v1 1.0)
                (Math/sqrt (- 1 v0v0v1v1))
                0)]
        (if negative?
          [[x y (unchecked-negate z)] byte-buffer]
          [[x y z] byte-buffer])))))

(defn decode-string [{:keys [flags num-bits] :as property} byte-buffer]
  (let [len (bit-buf/read-bits byte-buffer 9)]
    (bit-buf/read-bits byte-buffer (* len 8))))

(defn decode-int-64 [{:keys [flags num-bits] :as property} byte-buffer]
  (if (bit-and flags (bit-shift-left 1 19))
    (if (bit-and flags (bit-shift-left 1 0))
      (bit-buf/read-var-int-64 byte-buffer)
      (bit-buf/read-signed-var-int-64 byte-buffer))
    (if (bit-and flags (bit-shift-left 1 0))
      (let [neg? (bit-buf/read-bool byte-buffer)
            v (bit-buf/read-bits byte-buffer 63)]
        (if neg?
          (unchecked-negate v)
          v))
      (bit-buf/read-bits byte-buffer 64))))

(defn read-property [property byte-buffer]
  (case (:type property)
    0 (decode-int property byte-buffer)
    1 (decode-float property byte-buffer)
    2 (decode-vector property byte-buffer)
    3 (decode-vector-xy property byte-buffer)
    4 (decode-string property byte-buffer)
    5 (let [{:keys [array-element num-elements]} property
            num-bits (Integer/bitCount num-elements)
            num-elements-to-read (bit-buf/read-bits byte-buffer num-bits)]
        (loop [acc []
               byte-buffer byte-buffer]
          (if (>= (count acc) num-elements-to-read)
            acc
            (let [[v byte-buffer] (read-property array-element byte-buffer)]
              (recur (conj acc v) byte-buffer)))))
    6 nil
    7 (decode-int-64 property byte-buffer)
    (throw (RuntimeException. (str "unknown property type " (:type property))))))

(defn read-new-entity [class-id classes byte-buffer]
  (let [clazz (first (filter (comp (partial = class-id) :class-id) classes))
        new-way (bit-buf/read-bool byte-buffer)
        field-indicies (read-field-indicies new-way byte-buffer)
        fields (map (partial nth (:properties clazz)) field-indicies)]
    (loop [vals {}
           fields-rem fields
           byte-buffer byte-buffer]
      (if (empty? fields-rem)
        vals
        (let [prop (first fields-rem)
              v (read-property prop byte-buffer)]
          (recur (assoc-in vals (conj (:path prop) (:var-name prop)) v) (rest fields-rem) byte-buffer))))))

(defn handle-packet-entities [packet-entities-cmd demo-data handler-fns]
  (let [entry-count (:updated-entries packet-entities-cmd)]
    (loop [acc []
           header-base -1
           entries-remaining (dec entry-count)
           byte-buffer (bit-buf/bit-buffer (.asReadOnlyByteBuffer (:entity-data packet-entities-cmd)))]
      (let [is-entity (>= entries-remaining 0)
            entity-id-diff (if is-entity
                             (bit-buf/read-unsigned-bit-var byte-buffer)
                             nil)
            entity-id (if is-entity
                        (+ header-base 1 entity-id-diff))
            header-base (if is-entity
                          entity-id
                          header-base)
            update-type (if is-entity
                          (get-update-type byte-buffer)
                          :finish)
            update-type (if (or (not is-entity) (> entity-id 9999))
                          :finish
                          update-type)]
        (case update-type
          :enter (let [class-id (bit-buf/read-bits byte-buffer (int-log2 (count (:classes demo-data))))
                       serial-num (bit-buf/read-bits byte-buffer 10)]
                   (recur (conj acc (read-new-entity class-id (:classes demo-data) byte-buffer)) header-base (dec entries-remaining) byte-buffer))
          :leave (do
                   (throw (RuntimeException. "leave"))
                   (recur acc header-base (dec entries-remaining) byte-buffer))
          :delta (do
                   (throw (RuntimeException. "delta"))
                   (recur acc header-base (dec entries-remaining) byte-buffer))
          :finish acc
          (throw (RuntimeException. (str "Incorrect update-type " update-type))))))))

(defn read-demo
  ([fname]
   (read-demo fname {:demo-header println :packet-cmds {26 handle-packet-entities}}))
  ([fname {demo-header-fn :demo-header :as handler-fns}]
   (with-open [is (io/input-stream fname)]
     (demo-header-fn (read-demo-header is))
     (read-demo-cmds is handler-fns))))
