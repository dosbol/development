(ns discourje.core.async.buffers
  (:refer-clojure :exclude [type]))

(deftype Buffer [type n])

(defn buffer?
  "Check if value is of type Buffer"
  [x]
  (= (clojure.core/type x) Buffer))

(defn fixed-buffer
  "Create a fixed buffer where n > 0 elements."
  [n]
  {:pre [(> n 0)]}
  (->Buffer :fixed-buffer n))

(defn dropping-buffer
  "Create a dropping buffer where n > 0 elements."
  [n]
  {:pre [(> n 0)]}
  (->Buffer :dropping-buffer n))

(defn sliding-buffer
  "Create a sling buffer where n > 0 elements."
  [n]
  {:pre [(> n 0)]}
  (->Buffer :sliding-buffer n))

;(defn promise-buffer []
;  {:pre [true]}
;  (->Buffer :promise-buffer 0))

(defn n
  "Get the number of elements that the buffer holds
  (access n field of Buffer type)"
  [buffer]
  {:pre [(buffer? buffer)]}
  (.-n buffer))

(defn type
  "Get the type of the buffer
  (access type field of Buffer type)"
  [buffer]
  {:pre [(buffer? buffer)]}
  (.-type buffer))

;(defn unblocking-buffer? [buffer]
;  {:pre [(buffer? buffer)]}
;  (contains? #{:dropping-buffer :sliding-buffer :promise-buffer} (.-type buffer)))