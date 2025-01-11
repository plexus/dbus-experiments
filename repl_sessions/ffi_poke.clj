(ns repl-sessions.ffi-poke
  (:import
   org.freedesktop.dbus.connections.impl.DBusConnection
   org.freedesktop.dbus.connections.impl.DBusConnectionBuilder
   org.freedesktop.dbus.exceptions.DBusException
   org.freedesktop.dbus.interfaces.DBusInterface
   org.slf4j.LoggerFactory
   ch.qos.logback.classic.Level
   java.lang.reflect.Type
   org.apache.commons.lang3.reflect.TypeUtils
   org.freedesktop.Secrets
   org.freedesktop.SecretsImpl))

(.. (LoggerFactory/getILoggerFactory)
    (getLogger "io.methvin.watcher.DirectoryWatcher")
    (setLevel (Level/toLevel "WARN")))

(def conn-build
  (.build
   (DBusConnectionBuilder/forSessionBus)))
#_
(gen-interface
 :name org.freedesktop.Secrets
 :extends [org.freedesktop.dbus.interfaces.DBusInterface]
 :methods
 [[OpenSession [] String]
  [CreateCollection [String Boolean] void]
  [LockService [] void]
  [SearchCollections [java.util.Map] org.freedesktop.dbus.Tuple]
  [RetrieveSecrets [java.util.List] java.util.List]])

#_(def secret-service
    (reify org.freedesktop.Secrets
      (isRemote [this] false)

      ;; OpenSession (OUT ObjectPath result)
      (OpenSession [this]
        (prn "OPEN SESSION"))
      ;; CreateCollection  (IN  String               label,
      ;;                    IN  Boolean              private)
      (CreateCollection [this label private?]
        (prn "CREATE COLLECTION" label private?))
      ;; LockService ()
      (LockService [this]
        (prn "LOCK SERVICE"))
      ;; SearchCollections (IN  Dict<String,String>  fields,
      ;;                    OUT Array<ObjectPath>    results,
      ;;                    OUT Array<ObjectPath>    locked)
      (SearchCollections [this fields]
        (prn "SEARCH COLLECTIONS" fields))
      ;; RetrieveSecrets   (IN  Array<ObjectPath>    items,
      ;;                        OUT Array<Secret>        secrets)
      (RetrieveSecrets [this items]
        (prn "RETRIEVE SECRETS" items))))

(def secret-service
  (SecretsImpl.
   (fn []
     (prn "OPEN"))
   (fn [label private?]
     (prn "CREATE" label private?))
   (fn []
     (prn "LOCK"))
   (fn [fields]
     (prn "SEARCH" fields))
   (fn [items]
     (prn "RETRIEVE"))
   ))

#_
(clojure.reflect/reflect secret-service)

(.requestBusName conn-build "org.freedesktop.secrets")
(.exportObject conn-build "/" secret-service)
