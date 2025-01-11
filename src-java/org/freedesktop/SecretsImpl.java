package org.freedesktop;

import clojure.lang.IFn;
import java.util.List;
import java.util.Map;
import org.freedesktop.Secrets;
import org.freedesktop.dbus.DBusPath;

public class SecretsImpl implements Secrets {
    IFn open = null;
    IFn create = null;
    IFn lock = null;
    IFn search = null;
    IFn retrieve = null;

    public SecretsImpl(IFn open, IFn create, IFn lock, IFn search, IFn retrieve) {
        this.open = open;
        this.create = create;
        this.lock = lock;
        this.search = search;
        this.retrieve = retrieve;
    }

    @Override
    public boolean isRemote() {
        return false;
    }

    @Override
    public String getObjectPath() {
        return "/";
    }

    @Override
    public String OpenSession() {
        return (String)open.invoke();
    }

    @Override
    public void CreateCollection(String label, boolean isPrivate) {
        create.invoke(label, isPrivate);
    }

    @Override
    public void LockService() {
        lock.invoke();
    }

    @Override
    public Secrets.CollectionResponse SearchCollections(Map<String, String> fields) {
        return (Secrets.CollectionResponse)search.invoke(fields);
    }

    // @Override
    // public List<Secrets.Secret> RetrieveSecrets(List<DBusPath> items) {
    //     return (List<Secrets.Secret>)retrieve.invoke(items);
    // }
}
