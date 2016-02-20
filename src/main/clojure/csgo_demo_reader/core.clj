(ns csgo-demo-reader.core
  (:require [clojure.java.io :as io]
            [octet.core :as buf]
            [csgo-demo-reader.spec :as spec]
            [csgo-demo-reader.util :as util])
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

(defn ret-inc-pos [pos v]
  (apply conj [(+ pos (first v))] (rest v)))

(defn read-demo-packet [input-stream]
  (let [packet-size (read-packet-size input-stream)]
    (loop [num-bytes-read 0
           acc []]
      (if (<= packet-size num-bytes-read)
        acc
        (let [[num-bytes-read cmd] (ret-inc-pos num-bytes-read (spec/read-var-int32 input-stream))
              [num-bytes-read size] (ret-inc-pos num-bytes-read (spec/read-var-int32 input-stream))
              cmd-proto (get commands cmd)
              num-bytes-read (+ num-bytes-read size)]
          (if cmd-proto
            (let [b (buf/allocate size)]
              (.read input-stream (.array b))
              (recur num-bytes-read (conj acc (protobuf-load cmd-proto (.array b)))))
            (do
              (swap! unknown-msg-count inc)
              (util/safe-skip input-stream size)
              (recur num-bytes-read acc))))))))

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
