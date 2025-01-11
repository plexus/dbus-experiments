package org.freedesktop;

import java.util.List;
import java.util.Map;

import org.freedesktop.dbus.DBusPath;
import org.freedesktop.dbus.Struct;
import org.freedesktop.dbus.Tuple;
import org.freedesktop.dbus.annotations.Position;
import org.freedesktop.dbus.interfaces.DBusInterface;

public interface Secrets extends DBusInterface {
    public class Secret extends Struct {
        @Position(0)
        public String algorithm;

        @Position(1)
        public List<Byte> parameters;

        @Position(2)
        public List<Byte> value;
    }

    public class CollectionResponse extends Tuple {
        @Position(0)
        public List<DBusPath> results;
        @Position(1)
        public List<DBusPath> locked;
    }

    public String OpenSession();
    public void CreateCollection(String label, boolean isPrivate);
    public void LockService();
    public CollectionResponse SearchCollections(Map<String, String> fields);
    // public List<Secret> RetrieveSecrets(List<DBusPath> items);
}
