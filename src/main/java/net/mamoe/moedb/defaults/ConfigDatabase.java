package net.mamoe.moedb.defaults;

import cn.nukkit.utils.Config;
import cn.nukkit.utils.ConfigSection;
import net.mamoe.moedb.Database;

import java.util.LinkedHashMap;

/**
 * {@link ConfigSection} 存储在本机内存的数据库
 *
 * @author Him188 @ MoeDB Project
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class ConfigDatabase extends ConfigSection implements Database<String, Object> {
    public static final String NAME = "Config";

    @Override
    public String getName() {
        return NAME;
    }

    public ConfigDatabase() {
        super();
    }

    public ConfigDatabase(String key, Object value) {
        super(key, value);
    }

    public ConfigDatabase(LinkedHashMap<String, Object> map) {
        super(map);
    }

    public ConfigDatabase(Config nukkitConfig) {
        super();
        putAll(nukkitConfig.getAll());
    }
}
