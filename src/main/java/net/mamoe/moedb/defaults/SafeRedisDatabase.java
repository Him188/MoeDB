package net.mamoe.moedb.defaults;

import redis.clients.jedis.JedisShardInfo;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.*;

/**
 * 与 {@link RedisDatabase} 不同的是, 本类可对值进行转换, 如将 <code>Map<?, ?></code> 转换为 <code>Map<String, Object></code>, 以减少其他插件调用代码
 *
 * @author Him188 @ MoeDB Project
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class SafeRedisDatabase extends RedisDatabase {
    public static String NAME = "SafeRedis";

    public SafeRedisDatabase(String host, int port, String user, String password) throws JedisConnectionException {
        super(host, port, user, password);
    }

    public SafeRedisDatabase(JedisShardInfo info) throws JedisConnectionException {
        super(info);
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public boolean containsValue(final Object value) throws IllegalArgumentException {
        return super.containsValue(valueCast(value));
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object put(String key, final Object value) throws ClassCastException {
        return super.put(key, valueCast(value));
    }

    @Override
    public Object remove(Object key) {
        return super.remove(valueCast(key));
    }

    private static Object valueCast(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof Map) {
            return new HashMap<String, String>() {
                {
                    ((Map<?, ?>) value).forEach((key, value) -> put(key.toString(), String.valueOf(value)));
                }
            };
        } else if (value instanceof List) {
            return new ArrayList<String>(((List<?>) value).size()) {
                {
                    for (Object o : ((List<?>) value)) {
                        add(o.toString());
                    }
                }
            };
        } else if (value instanceof Set) {
            return new HashSet<String>(((Set<?>) value).size()) {
                {
                    for (Object o : ((Set<?>) value)) {
                        add(o.toString());
                    }
                }
            };
        } else {
            return value;
        }
    }

    @Override
    public ChildDatabase getChildDatabase(String key) {
        if (key == null) {
            throw new NullPointerException("key");
        }
        if (!getClient().type(key).equals("map")) {
            throw new IllegalArgumentException("type of key is not map");
        }
        return new ChildDatabase(this, key);
    }

    public static class ChildDatabase extends RedisDatabase.ChildDatabase{
        private ChildDatabase(RedisDatabase parentDatabase, String parentKey) {
            super(parentDatabase, parentKey);
        }

        @Override
        public void putAll(Map<? extends String, ?> m) {
            database.getClient().del(parentKey);
            database.getClient().hmset(parentKey, new HashMap<String, String>(){
                {
                    for (Entry<? extends String, ?> entry : m.entrySet()) {
                        this.put(entry.getKey(), String.valueOf(entry.getValue()));
                    }
                }
            });
        }
    }

}
