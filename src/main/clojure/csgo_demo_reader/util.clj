(ns csgo-demo-reader.util)

(defn safe-skip [input-stream num-bytes]
  (loop [pos 0]
    (if (<= num-bytes pos)
      pos
      (recur (+ pos (.skip input-stream (- num-bytes pos)))))))

(defn safe-read [input-stream buff]
  (loop [pos 0]
    (if (<= (alength buff) pos)
      pos
      (recur (+ pos (.read input-stream buff pos (- (alength buff) pos)))))))
