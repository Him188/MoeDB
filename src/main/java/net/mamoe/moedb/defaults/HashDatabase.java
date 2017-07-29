package net.mamoe.moedb.defaults;

import cn.nukkit.utils.Config;
import net.mamoe.moedb.AbstractDatabase;
import net.mamoe.moedb.Database;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 存储在内存的哈希链表数据库
 *
 * @author Him188 @ MoeDB Project
 * @see LinkedHashMap
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class HashDatabase extends AbstractDatabase<String, Object> implements Database<String, Object>, Cloneable, Serializable {
    public static final String NAME = "Config";

    @Override
    public String getName() {
        return NAME;
    }

    public HashDatabase() {
        super();
    }

    public HashDatabase(LinkedHashMap<String, ?> map) {
        super(map);
    }

    public HashDatabase(Config nukkitConfig) {
        super();
        putAll(nukkitConfig.getAll());
    }

    @Override
    public Object get(Object key) {
        return super.get(key);
    }

    @SuppressWarnings("unchecked")
    @Override
    public HashDatabase getChildDatabase(String key) {
        return new HashDatabase(new LinkedHashMap<>((Map<String, ?>) super.getRawMap(key)));
    }

    @Override
    public HashDatabase clone() {
        return (HashDatabase) super.clone();
    }
}
