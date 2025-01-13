(ns dbus-message-format
  (:require
   [clojure.java.io :as io])
  (:import
   (java.net ServerSocket StandardProtocolFamily UnixDomainSocketAddress)
   (java.nio ByteBuffer ByteOrder)
   (java.nio.channels ServerSocketChannel SocketChannel)))

(set! *warn-on-reflection* true)

(declare get-byte get-signature get-string get-string get-uint32
         put-byte put-signature put-string put-string put-uint32)

(def types
  [{:id :bool :sig \b}
   {:id :byte :sig \y :read get-byte :write put-byte}
   {:id :double :sig \d}
   {:id :int16 :sig \n}
   {:id :int32 :sig \i}
   {:id :int64 :sig \x}
   {:id :object-path :sig \o :read get-string :write put-string}
   {:id :signature :sig \g :read get-signature :write put-signature}
   {:id :string :sig \s :read get-string :write put-string}
   {:id :uint16 :sig \q}
   {:id :uint32 :sig \u :read get-uint32 :write put-uint32}
   {:id :uint64 :sig \t}
   {:id :array :sig \a :read get-array :write put-array}
   {:id :variant :sig \v}
   {:id :struct :sig \( :sig-end\)}])

(def message-types
  [:invalid :method-call :method-return :error :signal])

(def headers
  [[:invalid nil]
   [:path :object-path]
   [:interface :string]
   [:member :string]
   [:error-name :string]
   [:reply-serial :uint32]
   [:destination :string]
   [:sender :string]
   [:signature :signature]
   [:unix-fds :uint32]])

(def sig->type* (into {} (map (juxt :sig :id)) types))
(def type->sig* (into {} (map (juxt :id :sig)) types))
(def type->write-fn* (into {} (map (juxt :id :write)) types))
(def type->read-fn* (into {} (map (juxt :id :read)) types))

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
  (let [len (get-byte buf)
        res (String. ^bytes (let [ba (byte-array len)]
                              (.get buf ba 0 len)
                              ba))]
    (.get buf) ;; NULL
    res))


(defn read-type [buf t]
  ((or (type->read-fn* t)
       (cond
         (vector? t)
         (case (first t)
           :tuple
           (if (= 1 (count t))
             (constantly nil)
             (apply juxt
                    (map (fn [t] #(read-type % t)) (rest t)))))

         :else
         (throw (ex-info  "unimplemented type" {:t t}))))
   buf))

(defn read-header [^ByteBuffer buf]
  (align buf 8)
  (let [code (get-byte buf)
        ;; _ (prn "<H" (.position buf) code)
        _ (assert (< 0 code 10) code)
        [h t] (nth headers code)
        sig (get-signature buf)
        v (read-type buf (sig->type sig))]
    ;; (prn [(.position buf) h t v])
    [h v]))


(defn type->sig [t]
  (if (vector? t)
    (case (first t)
      :tuple
      (apply str (map type->sig (rest t)))
      :array
      (str "a" (type->sig (second t)))
      :struct
      (str "(" (apply str (map type->sig (rest t))) ")")
      :variant
      "v")
    (str (type->sig* t))))

(defn sig->type [sig]
  (if (char? sig)
    (sig->type* sig)
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
          \a
          (recur
           (conj t [:array (sig->type (first cs))])
           (next cs))

          \v
          (let [[_tuple & ts] (sig->type cs)]
            (into (conj t [:variant (first ts)]) (next ts)))

          #_else
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
  ((or
    (type->write-fn* t)
    (cond
      (vector? t)
      (case (first t)
        :tuple
        (if (= 1 (count t))
          (constantly nil)
          (apply juxt
                 (map (fn [t] #(write-type % t)) (rest t))))
        :variant
        (fn [buf v]
          (put-signature buf (type->sig (second t)))
          (write-type buf (second t) v)))

      :else
      (throw (ex-info  "unimplemented type" {:t t}) )))
   buf v))

(defn write-headers [^ByteBuffer buf header-map]
  (let [hidx (into {} (map-indexed (fn [idx [k v]] [k idx]) headers))
        headers (into {} headers)]
    (doseq [[k v] header-map]
      (align buf 8)
      (let [t (get headers k)
            code (get hidx k)]
        ;; (println ["H" k code t v (.position buf)])
        (put-byte buf code)
        (write-type buf [:variant t] v)))
    buf))

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
    (align buf 4)
    buf))

(defn dbus-session-sock []
  (let [[_ path] (re-find #"unix:path=(.*)" (System/getenv "DBUS_SESSION_BUS_ADDRESS"))]
    (SocketChannel/open (UnixDomainSocketAddress/of ^String path))))

(comment
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
          (take 16 (map char bs)))))


(defn sock-read ^bytes [^SocketChannel chan]
  (let [buf (ByteBuffer/allocate 1024)]
    (.mark buf)
    (let [len (.read chan buf)]
      (if (< 0 len)
        (let [arr (byte-array len)]
          (.flip buf)
          (.get buf arr 0 len)
          arr)
        (do
          (println "WARN: read from closed channel")
          (byte-array 0))))))

(defn sock-write [^SocketChannel chan ^String s]
  (prn s)
  (let [buf (ByteBuffer/allocate 1024)]
    (.mark buf)
    (.put buf (.getBytes s))
    (.flip buf)
    (.write chan buf)))

(defn write-to-str [f & args]
  (let [^ByteBuffer b (ByteBuffer/allocate 1024)]
    (.mark b)
    (apply f b args)
    (let [len (.position b)
          arr (byte-array len)]
      (.flip b)
      (.get b arr 0 len)
      (String. arr))))

(comment
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

  (let [s (dbus-session-sock)]
    (sock-write s (str "\0" "AUTH\r\n"))
    (prn (String. (sock-read s))) ;; "REJECTED EXTERNAL\r\n"
    (sock-write s "AUTH EXTERNAL\r\n")
    (prn (String. (sock-read s))) ;; "DATA\r\n"
    (sock-write s "DATA\r\n")
    (prn (String. (sock-read s))) ;; "OK 18f436e3312715725ef4204b67810658\r\n"
    (sock-write s "NEGOTIATE_UNIX_FD\r\n")
    (prn (String. (sock-read s)))
    (sock-write s "BEGIN\r\n")

    (let [bs (byte-array 1024)
          b (ByteBuffer/wrap bs)]
      (.mark b)
      (.order b ByteOrder/LITTLE_ENDIAN)
      (def offset 0)
      (write-message b {:type :method-call,
                        :flags {},
                        :version 1,
                        :serial 1,
                        :headers
                        {:path "/org/freedesktop/DBus"
                         :member "Hello"
                         :interface "org.freedesktop.DBus"
                         :destination "org.freedesktop.DBus"
                         }})
      ;; (put-byte b 0)
      ;; (put-byte b 0)
      ;; (put-byte b 0)
      (.flip b)
      (prn (String. (into-array Byte/TYPE
                                (subvec (vec bs)
                                        (.position b)
                                        (.limit b)))))
      (.write s b))
    (prn (read-message (ByteBuffer/wrap (sock-read s)))))


  (char 108)
  (read-message
   (ByteBuffer/wrap arr 7 (- (count arr) 7)))


  (write-to-str
   write-message
   {:type :method-call,
    :flags {},
    :version 1,
    :serial 1,
    :headers
    {:path "/org/freedesktop/DBus"
     :member "Hello"
     :interface "org.freedesktop.DBus"
     :destination "org.freedesktop.DBus",
     }})

  (subs (String. arr) (+ 7 ;; BEGIN
                         ;; 12 ;; HEADER
                         ;; 4 ;; arr-len
                         ;; 1 ;; 3 MEMBER

                         ;; 2
                         )))

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

(comment
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
    (.get b)))

(comment
  ""
  "l " #_len "    " #_serial "   "
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



  (char 21) ;; => \
  (char 20)) ;; => \

(comment
  (=
   "l        m   o    /org/freedesktop/DBus   s    org.freedesktop.DBus    s    Hello   s    org.freedesktop.DBus    "
   "l        m   o    /org/freedesktop/DBus   s    org.freedesktop.DBus    s    Hello   s    org.freedesktop.DBus    ")

  (long \M) ;; => 77
  (long \m) ;; => 109
  (=
   "BEGIN\r\nl        m         /org/freedesktop/DBus         Hello         org.freedesktop.DBus          org.freedesktop.DBus    "
   "BEGIN\r\nl        m   s    /org/freedesktop/DBus   s    Hello   s    org.freedesktop.DBus    s    org.freedesktop.DBus    "
   "BEGIN\r\nl        m   o    /org/freedesktop/DBus   s    org.freedesktop.DBus    s    Hello   s    org.freedesktop.DBus    ")

  (read-message (ByteBuffer/wrap (.getBytes "l        m   o    /org/freedesktop/DBus   s    org.freedesktop.DBus    s    Hello   s    org.freedesktop.DBus    ")))

  (def offset 0)
  (count "l        m   o    /org/freedesktop/DBus   s    Hello   s    org.freedesktop.DBus    s    org.freedesktop.DBus    ")


  " AUTH\r\n"
  "REJECTED EXTERNAL\r\n"
  "AUTH EXTERNAL\r\n"
  "DATA\r\n"
  "DATA\r\n"
  "OK 18f436e3312715725ef4204b67810658\r\n"
  "NEGOTIATE_UNIX_FD\r\n"
  "AGREE_UNIX_FD\r\n"
  "BEGIN\r\n"
  "l        m   o    /org/freedesktop/DBus   s    Hello   s    org.freedesktop.DBus    s    org.freedesktop.DBus    "


  " AUTH\r\n"
  "REJECTED EXTERNAL\r\n"
  "AUTH EXTERNAL\r\n"
  "DATA\r\n"
  "DATA\r\n"
  "OK 18f436e3312715725ef4204b67810658\r\n"
  "NEGOTIATE_UNIX_FD\r\n"
  "AGREE_UNIX_FD\r\n"
  "BEGIN\r\n"

  (=
   "l        p   o    /org/freedesktop/DBus   s    Hello   s    org.freedesktop.DBus    s    org.freedesktop.DBus    "

   "l        m   o    /org/freedesktop/DBus   s    Hello   s    org.freedesktop.DBus    s    org.freedesktop.DBus    ")

  (- (long \p) (long \m)))
