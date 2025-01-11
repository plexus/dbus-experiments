(ns dbus-message-format
  (:require
   [clojure.java.io :as io])
  (:import
   (java.net ServerSocket StandardProtocolFamily UnixDomainSocketAddress)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.channels ServerSocketChannel SocketChannel)))

(set! *warn-on-reflection* true)

(def message-types
  [:invalid :method-call :method-return :error :signal])

(def headers
  [[:invalid nil]
   [:path :string]
   [:interface :string]
   [:member :string]
   [:error-name :string]
   [:reply-serial :uint32]
   [:destination :string]
   [:sender :string]
   [:signature :signature]
   [:unix-fds :uint32]])

(def offset 0)

(defn align [^ByteBuffer buf size]
  (dotimes [_ (mod (- size (mod (- (.position buf) offset) size)) size)]
    (.put buf (byte 0)))
  buf)

(defn get-byte [^ByteBuffer buf]
  (bit-and (.get buf) 0xff))

(defn get-uint32 [^ByteBuffer buf]
  (align buf 4)
  (bit-and (.getInt buf) 0xffffffff))

(defn get-array [^ByteBuffer buf read-fn]
  (align buf 4)
  (let [len (get-uint32 buf)
        end (+ (.position buf) len)]
    (loop [res []]
      (if (< (.position buf) end)
        (recur (conj res (doto (read-fn buf) prn)))
        res))))

(defn get-struct [^ByteBuffer buf read-fns]
  (align buf 8)
  (mapv #(% buf) read-fns))

(defn get-string [^ByteBuffer buf]
  (align buf 4)
  (let [len (get-uint32 buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba))]
    (.get buf) ;; NULL
    res))

(defn get-signature [^ByteBuffer buf]
  (prn "SIG" (.position buf))
  (let [len (get-byte buf)
        _ (prn "SIG" (.position buf) len (.get buf (.position buf)))
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba))]
    (.get buf) ;; NULL
    res))

(defn read-type [buf t]
  ((case t
     :byte get-byte
     :uint32 get-uint32
     :string get-string
     :signature (fn [buf]
                  (get-signature buf)
                  (get-signature buf))
     (cond
       (vector? t)
       (case (first t)
         :tuple
         (if (= 1 (count t))
           (constantly nil)
           (apply juxt
                  (map (fn [t] #(read-type % t)) (rest t)))))

       :else
       (throw (ex-info  "unimplemented type" {:t t}) )
       ))
   buf))

(defn read-header [^ByteBuffer buf]
  (align buf 8)
  (let [code (get-byte buf)
        _ (prn "<H" (.position buf) code)
        _ (assert (< 0 code 10) code)
        [h t] (nth headers code)
        v (read-type buf t)]
    (prn [(.position buf) h t v])
    [h v]))

(defn sig->type [sig]
  (if (char? sig)
    (case sig
      \y :byte
      \b :bool
      \n :int16
      \q :uint16
      \i :int32
      \u :uint32
      \x :int64
      \t :uint64
      \d :double
      \s :string
      \o :object-path
      \g :signature
      )
    (loop [t [:tuple]
           [c & cs] sig]
      (if (not c)
        t
        (case c
          \(
          (recur
           (conj t
                 (into [:struct] (next (sig->type
                                        (take-while (complement #{\)}) cs)))))
           (next (drop-while (complement #{\)}) cs)))
          (recur (conj t (sig->type c))
                 cs))))))

(defn read-message [^ByteBuffer buf]
  (let [endian (.get buf)
        _ (.order buf
                  (case (char endian)
                    \l
                    ByteOrder/LITTLE_ENDIAN
                    \B
                    ByteOrder/BIG_ENDIAN))
        msg-type (nth message-types (get-byte buf))
        flags (let [flags (get-byte buf)]
                (cond-> {}
                  (= 0x1 (bit-and flags 0x1))
                  (assoc :no-reply-expected true)
                  (= 0x2 (bit-and flags 0x2))
                  (assoc :no-auto-start true)
                  (= 0x4 (bit-and flags 0x4))
                  (assoc :allow-interactive-authorization true)))
        version (get-byte buf)
        len  (get-uint32 buf)
        serial  (get-uint32 buf)
        headers (into {} (get-array buf read-header))]
    {:endian (char endian)
     :type msg-type
     :flags flags
     :version version
     :body-length len
     :serial serial
     :headers headers
     :body (read-type buf (sig->type (get headers :signature)))}))

(comment
  (sig->type \g)
  (char 111)
  (def buf  (ByteBuffer/wrap
             (.readAllBytes (io/input-stream "/home/arne/tmp/dbus-secret.bin"))))
  (let [buf buf]
    (def offset (.position buf))
    (read-message buf)
    ))

(defn put-string [^ByteBuffer buf s]
  (align buf 4)
  (let [b (.getBytes ^String s)]
    (.putInt buf (count b))
    (.put buf b)
    (.put buf (byte 0))))

(defn put-signature [^ByteBuffer buf s]
  (let [b (.getBytes ^String s)]
    (.put buf (byte (count b)))
    (.put buf b)
    (.put buf (byte 0))))

(defn put-byte [^ByteBuffer buf v]
  (.put buf (byte (bit-and (long v) 0xff))))

(defn put-uint32 [^ByteBuffer buf v]
  (.putInt buf (int (bit-and (long v) 0xffffffff))))

(defn write-array [^ByteBuffer buf write-elements-fn]
  (align buf 4)
  (prn "ARRAY"  (.position buf))
  (let [size-pos (.position buf)]
    (.putInt buf 0)
    (write-elements-fn buf)
    (let [end-pos (.position buf)]
      (.position buf size-pos)
      (.putInt buf (- end-pos size-pos 4))
      (.position buf end-pos)))
  buf)

(defn show-buffer-lim [^ByteBuffer b]
  (let [p (.limit b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn show-buffer-pos [^ByteBuffer b]
  (let [p (.position b)]
    (.position b 0)
    (repeatedly p #(.get b))))

(defn write-type [buf t v]
  ((case t
     :byte put-byte
     :uint32 put-uint32
     :string put-string
     :signature put-signature
     (cond
       (vector? t)
       (case (first t)
         :tuple
         (if (= 1 (count t))
           (constantly nil)
           (apply juxt
                  (map (fn [t] #(write-type % t)) (rest t)))))

       :else
       (throw (ex-info  "unimplemented type" {:t t}) )
       ))
   buf
   v))

(defn write-headers [^ByteBuffer buf header-map]
  (let [hidx (into {} (map-indexed (fn [idx [k v]] [k idx]) headers))
        headers (into {} headers)]
    (doseq [[k v] header-map]
      (align buf 8)
      (let [t (get headers k)
            code (get hidx k)]
        (println ["H" k code t v (.position buf)])
        (put-byte buf code)
        (write-type buf t v)))))

(defn write-message [^ByteBuffer buf {:keys [type flags headers version serial]
                                      :or {version 1}}]
  (let [endian (.order buf)]
    (put-byte buf (if (= endian ByteOrder/LITTLE_ENDIAN)
                    \l
                    \B))
    (put-byte buf (.indexOf ^java.util.List message-types type))
    (put-byte buf (cond-> 0
                    (:no-reply-expected flags)
                    (bit-or 1)
                    (:no-auto-start flags)
                    (bit-or 2)
                    (:allow-interactive-authorization flags)
                    (bit-or 3)))
    (put-byte buf version)

    (put-uint32 buf 0) ;;body length
    (put-uint32 buf serial)

    (write-array buf #(write-headers % headers))
    buf
    ))

(defn send-mes)

(.indexOf ^java.util.List message-types type)

(defn dbus-session-sock []
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (SocketChannel/open (UnixDomainSocketAddress/of ^String path))))

(let [buf (ByteBuffer/allocate 1024)]
  (.order buf ByteOrder/LITTLE_ENDIAN)
  (.mark buf)
  #_#_(put-byte buf 0)
  (.put buf (.getBytes (str "AUTH EXTERNAL 31303030"
                            #_                            (apply str (map (partial format "%02x")
                                                                          (map long (str (.pid
                                                                                          (ProcessHandle/current))))))
                            "\r\n"
                            )))
  (write-message buf
                 {:type :method-call,
                  :flags {},
                  :version 1,
                  :serial 1,
                  :headers
                  {:path "/org/freedesktop/DBus"
                   :member "Hello"
                   :interface "org.freedesktop.DBus"
                   :destination "org.freedesktop.DBus"}})
  (.flip buf)
  (.write chan buf)#_#_
  (def buf buf)
  (read-message buf)
  )
offset
(.limit buf)
(let [bs (drop 12 (show-buffer-lim buf))]
  (map  vector
        (take 16 bs)
        (take 16 (map char bs))))


(defn sock-read ^bytes [^SocketChannel chan]
  (let [buf (ByteBuffer/allocate 1024)]
    (.mark buf)
    (let [len (.read chan buf)
          arr (byte-array len)]
      (.flip buf)
      (.get buf arr 0 len)
      arr)))

(defn sock-write [^SocketChannel chan s]
  (let [buf (ByteBuffer/allocate 1024)]
    (.mark buf)
    (.put buf (.getBytes s))
    (.flip buf)
    (.write chan buf)))

(def server-sock
  (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
    (.bind (UnixDomainSocketAddress/of "/home/arne/tmp/dbus-socket"))))

(def sock
  (.accept server-sock))


(def arr
  (sock-read sock))

(sock-write sock "REJECTED EXTERNAL\r\n")
(sock-write sock "DATA\r\n")
(sock-write sock "OK 18f436e3312715725ef4204b67810658\r\n")
(sock-write sock "AGREE_UNIX_FD\r\n")
(String. (sock-read sock))
(subs (String. arr) (+ 12))
(map-indexed vector (map (juxt long char) (drop 7 arr)))


(long \m)

;;
(let [s (dbus-session-sock)]
  (sock-write s (str "\0" "AUTH\r\n"))
  (println (String. (sock-read s))) ;; "REJECTED EXTERNAL\r\n"
  (sock-write s "AUTH EXTERNAL\r\n")
  (println (String. (sock-read s))) ;; "DATA\r\n"
  (sock-write s "DATA\r\n")
  (println (String. (sock-read s))) ;; "OK 18f436e3312715725ef4204b67810658\r\n"
  (sock-write s "NEGOTIATE_UNIX_FD\r\n")
  (println (String. (sock-read s)))
  (sock-write s "BEGIN\r\n")
  (let [b (ByteBuffer/allocate 1024)]
    (.order b ByteOrder/LITTLE_ENDIAN)
    (write-message b {:type :method-call,
                      :flags {},
                      :version 1,
                      :serial 1,
                      :headers
                      {:path "/org/freedesktop/DBus"
                       :member "Hello"
                       :interface "org.freedesktop.DBus"
                       }})
    (.flip b)
    (.write s b))
  (println (String. (sock-read s)))
  )
;; => Reflection warning, /home/arne/dbus-test/src/dbus_message_format.clj:343:3 - call to java.lang.String ctor can't be resolved.
;;    Reflection warning, /home/arne/dbus-test/src/dbus_message_format.clj:345:3 - call to java.lang.String ctor can't be resolved.
;;    Reflection warning, /home/arne/dbus-test/src/dbus_message_format.clj:347:3 - call to java.lang.String ctor can't be resolved.
;;    "OK 18f436e3312715725ef4204b67810658\r\n"
(char 108)
(read-message
 (ByteBuffer/wrap arr 7 (- (count arr) 7)))

(defn write-to-str [f & args]
  (let [b (ByteBuffer/allocate 1024)]
    (.mark b)
    (apply f b args)
    (let [len (.position b)
          arr (byte-array len)]
      (.flip buf)
      (.get buf arr 0 len)
      (String. arr))))
[(write-to-str
  write-message
  {:type :method-call,
   :flags {},
   :version 1,
   :serial 1,
   :headers
   {:path "/org/freedesktop/DBus"
    :member "Hello"
    :interface "org.freedesktop.DBus"
    }})]
(subs (String. arr) (+ 7 ;; BEGIN
                       ;; 12 ;; HEADER
                       ;; 4 ;; arr-len
                       ;; 1 ;; 3 MEMBER

                       ;; 2
                       ))

"l " ;;h1
"    " ;;h2
"   " ;;h3
"m   " ;;arr
"" "       "
"e       ""   ""   org.freedesktop.DBus    "
"o "
"   /org/freedesktop/DBus "
"  s "
"   org.freedesktop.DBus "
"   "
""
(char 6)

(let [^SocketChannel in (.accept ^ServerSocketChannel server-sock)
      ^SocketChannel out (dbus-session-sock)]
  (let [bs (byte-array 1024)
        ^ByteBuffer b (doto (ByteBuffer/wrap bs) (.mark))
        show-bytes (fn [c]
                     (println c (.position b) (pr-str (String. ^bytes (into-array Byte/TYPE (take (.position b) bs)) "ASCII")))
                     )]

    (while true
      (Thread/sleep 300)
      (.read in b)
      (show-bytes "> ")
      (.flip b)

      (.write out b)
      (.clear b)
      (Thread/sleep 300)
      (.read out b)
      (show-bytes "< ")
      (.flip b)

      (.write in b)
      (.clear b)
      )
    )
  )

(let [b (ByteBuffer/allocate 100)]
  (.mark 0)
  (.put b 1)
  (.flip b)
  (.get b))
""
"l " #_len "    " #_seriel "   "
"m   "
A_
__S1"" "o "
__S2__ "   /org/freedesktop/DBus   "
__S1"""s "
__S2__"   org.freedesktop.DBus    "
__S3"""s "
__S3__"   Hello " "  "
__S4"s "
__S3__"   org.freedesktop.DBus    "
""
"l        " "m   "
"   "
"   /org/freedesktop/DBus         Hello         org.freedesktop.DBus          org.freedesktop.DBus "

(count "/org/freedesktop/DBus")


(char 21) ;; => \
(char 20) ;; => \
