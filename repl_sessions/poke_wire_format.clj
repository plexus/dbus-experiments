(ns repl-sessions.poke-wire-format
  (:require
   [thirdparty.marshal :as m])
  (:import
   (java.net UnixDomainSocketAddress)
   (java.nio ByteBuffer)
   (java.nio.channels SocketChannel)))

;;declare a marshaling struct in the same order and using the same types as the C struct
(def s (m/struct :type m/sint32 :name (m/ascii-string 10) :data (m/array m/sint32 5)))

;;create an output stream (in lieu of an outputstream from a socket)
(def os (java.io.ByteArrayOutputStream.))

;;marshal a clojure map to the outputstream
(m/write os s {:type 1 :name "1234567890" :data [1 2 3 4 5]})
34

;;create an input stream (in lieu of an inputstream from a socket)
(def is (clojure.java.io/input-stream (.toByteArray os)))

;;marshal the clojure map from the input stream
(m/read is s)
{:type 1, :name "1234567890", :data [1 2 3 4 5]}

;; 1st BYTE	Endianness flag; ASCII 'l' for little-endian or ASCII 'B' for big-endian. Both header and body are in this endianness.
;; 2nd BYTE	Message type. Unknown types must be ignored. Currently-defined types are described below.
;; 3rd BYTE	Bitwise OR of flags. Unknown flags must be ignored. Currently-defined flags are described below.
;; 4th BYTE	Major protocol version of the sending application. If the major protocol version of the receiving application does not match, the applications will not be able to communicate and the D-Bus connection must be disconnected. The major protocol version for this version of the specification is 1.
;; 1st UINT32	Length in bytes of the message body, starting from the end of the header. The header ends after its alignment padding to an 8-boundary.
;; 2nd UINT32	The serial of this message, used as a cookie by the sender to identify the reply corresponding to this request. This must not be zero.
;; ARRAY of STRUCT of (BYTE,VARIANT)	An array of zero or more header fields where the byte is the field code, and the variant is the field value. The message type determines which fields are required.


;; INVALID	0	This is an invalid type.
;; METHOD_CALL	1	Method call. This message type may prompt a reply.
;; METHOD_RETURN	2	Method reply with returned data.
;; ERROR	3	Error reply. If the first argument exists and is a string, it is an error message.
;; SIGNAL	4	Signal emission.

(def chan
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (SocketChannel/open (UnixDomainSocketAddress/of path))))

(def buf
  (ByteBuffer/allocate 1024))

(clojure.reflect/reflect ByteBuffer)

(.read chan buf)

(let [buf   (ByteBuffer/allocate 1024)]
  ;; header
  (.put buf (byte \B)) ;; big endian
  (.put buf (byte 1)) ;; method call
  (.put buf (byte 0)) ;; flags
  (.put buf (byte 1)) ;; protocol version
  (.putInt buf (int 0)) ;; XXX length of the body
  (.putInt buf (int 1));; message id for response, serial, not zero

  ;; header fields
  (.putInt buf (int 0)) ;; XXX array length in bytes
  )

;; ARRAY
;;  A UINT32 giving the length of the array data in bytes, followed by alignment padding to the alignment boundary of the array element type, followed by each array element.

(def header-ids
  {:path 1
   :member 3})

(defn put-string [buf s]
  (let [b (.getBytes ^String s)]
    (.putInt buf (count b))
    (.put buf b)
    (.put buf (byte 0))))

(defn align [buf size]
  (dotimes [_ (mod (- size (mod (.position buf) size)) size)]
    (.put buf (byte 0)))
  buf)

(defn put-array [buf a]
  (align buf 4)
  (let [size-pos (.position buf)]
    (.putInt buf 0)
    (run! (partial put buf) a)
    (let [end-pos (.position buf)]
      (.position buf size-pos)
      (.putInt buf (- end-pos size-pos 4))
      (.position buf end-pos)
      ))
  )

(declare put)

(defn put-struct [buf s]
  (align buf 8)
  (run! (partial put buf) s)
  buf)

(defn put [b v]
  (cond
    (string? v)
    (put-string b v)

    (vector? v)
    (case (first v)
      :struct
      (put-struct b (next v))
      :array
      (put-array b (next v))
      :byte
      (.put b (byte (bit-and (second v) 0xff)))
      :uint32
      (.putInt b (int (bit-and (second v) 0xffffffff)))
      ))

  b)

(count (.getBytes "hello"))

(show-buffer
 (put-string-array (ByteBuffer/allocate 100) ["hello"])
 )

(show-buffer
 (put (ByteBuffer/allocate 100)
      [:array
       [:struct
        [:byte 1]
        "/org/freedesktop/DBus"]
       [:struct
        [:byte 3]
        "Hello"]]))

(let [b (ByteBuffer/allocate 100)]
  (.putInt b 1)
  (.position b 0)
  [(.get b)(.get b)(.get b)(.get b)(.get b)])

(defn show-buffer [b]
  (let [p (.position b)]
    (.position b 0)
    (repeatedly p #(.get b))))
