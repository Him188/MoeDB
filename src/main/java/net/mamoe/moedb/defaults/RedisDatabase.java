package net.mamoe.moedb.defaults;

import cn.nukkit.utils.ConfigSection;
import net.mamoe.moedb.Database;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisShardInfo;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.*;
import java.util.Map.Entry;

/**
 * Redis 数据库 <br>
 * <a href="https://baike.baidu.com/item/Redis/6549233?fr=aladdin">查看百度百科介绍</a> <br>
 * 本类不会对值类型进行转换, 值类型有误时将会抛出异常({@link IllegalArgumentException}) <br>
 * 若需自动转换, 请使用 {@link SafeRedisDatabase}
 *
 * @author Him188 @ MoeDB Project
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class RedisDatabase extends ConfigSection implements Database<String, Object> {
    public static String NAME = "Redis";

    private final Jedis client;

    public RedisDatabase(String host, int port, String user, String password) throws JedisConnectionException {
        JedisShardInfo info = new JedisShardInfo(host, port, user);
        info.setPassword(password);
        client = new Jedis(info);
        client.connect();
    }

    public RedisDatabase(JedisShardInfo info) throws JedisConnectionException {
        client = new Jedis(info);
        client.connect();
    }

    public Jedis getClient() {
        return client;
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public int size() {
        return Math.toIntExact(client.dbSize());
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    @Override
    public boolean containsKey(Object key) {
        return !client.type(Objects.requireNonNull(key).toString()).equals("none");
    }

    /**
     * 判断值 <code>value</code> 是否存在 <br>
     * 值的类型只能是 <code>Map<String, String></code> 或 <code>List<String></code> 或 <code>String</code> 或 <code>Set<String></code>,
     * 否则将抛出异常 {@link IllegalArgumentException}
     *
     * @param value 需要查询的值
     *
     * @return 值 <code>value</code> 是否存在
     *
     * @throws IllegalArgumentException 当参数 <code>value</code> 的类型无效时抛出
     */
    @Override
    public boolean containsValue(Object value) throws IllegalArgumentException {
        if (value == null || (!(value instanceof Map) && !(value instanceof String) && !(value instanceof List) && !(value instanceof Set))) {
            return false;
        }

        Set<String> keys = client.keys(".");
        for (String key : keys) {
            switch (client.type(key)) {
                case "none":
                    continue;
                case "map":
                    if (client.hgetAll(key).equals(value)) {
                        return true;
                    }
                    continue;
                case "set":
                    if (client.zrange(key, 0, -1).equals(value)) {
                        return true;
                    }
                    continue;
                case "list":
                    if (client.lrange(key, 0, -1).equals(value)) {
                        return true;
                    }
                    continue;
                case "string":
                    if (client.get(key).equals(value)) {
                        return true;
                    }
                    continue;
                default:
            }
        }

        return false;
    }

    @Override
    public Object get(String key) {
        switch (client.type(key)) {
            case "none":
                return null;
            case "map":
                return client.hgetAll(key);
            case "set":
                return client.zrange(key, 0, -1);
            case "list":
                return client.lrange(key, 0, -1);
            case "string":
                return client.get(key);
            default:
                return null;
        }
    }

    /**
     * 判断值 <code>value</code> 是否存在 <br>
     * 值的类型只能是 <code>Map<String, String></code> 或 <code>List<String></code> 或 <code>String</code> 或 <code>Set<String></code>,
     * 否则将抛出异常 {@link IllegalArgumentException}
     *
     * @param value 需要查询的值
     *
     * @return <code>key</code> 的旧值. 当 <code>key</code> 不存在时为 null
     *
     * @throws ClassCastException       当参数 <code>value</code> 的类型无效时抛出(参数类型属于 Map, List 或 Set, 但值类型不是 String)
     * @throws IllegalArgumentException 当参数 <code>value</code> 的类型无效时抛出(参数类型不属于 String, Map, List 或 Set)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object put(String key, Object value) throws ClassCastException, IllegalArgumentException {
        if (value instanceof String) {
            if (client.exists(key)) {
                return client.getSet(key, String.valueOf(value));
            } else {
                client.set(key, String.valueOf(value));
                return null;
            }
        }

        if (value instanceof Map) {
            Object old = client.exists(key) ? client.hgetAll(key) : null;
            client.hmset(key, (Map<String, String>) value);
            return old;
        }

        if (value instanceof List) {
            Object old = client.exists(key) ? client.lrange(key, 0, -1) : null;
            client.del(key);
            client.lpush(key, ((List<String>) value).toArray(new String[((List<String>) value).size()]));
            return old;
        }

        if (value instanceof Set) {
            Object old = client.exists(key) ? client.zrange(key, 0, -1) : null;
            client.del(key);
            Map<String, Double> map = new HashMap<>();
            for (String o : (Set<String>) value) {
                map.put(String.valueOf(o), 1D);
            }
            client.zadd(key, map);
            return old;
        }

        throw new IllegalArgumentException("argument value must be String,Map,List or set");
    }

    @Override
    public Object remove(Object key) {
        Object value = this.get(Objects.requireNonNull(key));
        client.del(String.valueOf(key));
        return value;
    }

    @Override
    public void putAll(Map<? extends String, ?> m) {
        for (Map.Entry<? extends String, ?> entry : m.entrySet()) {
            this.put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public void clear() {
        client.flushDB();
    }

    @Override
    public Set<String> keySet() {
        return client.keys(".");
    }

    @Override
    public Collection<Object> values() {
        Collection<Object> values = new ArrayList<>();

        Set<String> keys = client.keys(".");
        for (String key : keys) {
            switch (client.type(key)) {
                case "none":
                    continue;
                case "map":
                    values.add(client.hgetAll(key));
                    continue;
                case "set":
                    values.add(client.zrange(key, 0, -1));
                    continue;
                case "list":
                    values.add(client.lrange(key, 0, -1));
                    continue;
                case "string":
                    values.add(client.get(key));
                    continue;
                default:

            }
        }

        return values;
    }

    @Override
    public Set<Entry<String, Object>> entrySet() {
        Set<Entry<String, Object>> values = new HashSet<>();

        Set<String> keys = client.keys(".");
        for (String key : keys) {
            switch (client.type(key)) {
                case "none":
                    continue;
                case "map":
                    values.add(new SimpleEntry(key, client.hgetAll(key)));
                    continue;
                case "set":
                    values.add(new SimpleEntry(key, client.zrange(key, 0, -1)));
                    continue;
                case "list":
                    values.add(new SimpleEntry(key, client.lrange(key, 0, -1)));
                    continue;
                case "string":
                    values.add(new SimpleEntry(key, client.get(key)));
                    continue;
                default:

            }
        }

        return values;
    }

    private static class SimpleEntry implements Entry<String, Object> {
        private final String key;
        private Object value;

        public SimpleEntry(String key, Object value) {
            this.key = key;
            this.value = value;
        }

        @Override
        public final String getKey() {
            return this.key;
        }

        @Override
        public final Object getValue() {
            return this.value;
        }

        @Override
        public final Object setValue(Object value) {
            return this.value = value;
        }

        @Override
        public final String toString() {
            return key + "=" + value;
        }

        @Override
        public final int hashCode() {
            return Objects.hashCode(key) ^ Objects.hashCode(value);
        }

        @Override
        public final boolean equals(Object o) {
            if (o == this)
                return true;
            if (o instanceof Map.Entry) {
                Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
                if (Objects.equals(key, e.getKey()) &&
                    Objects.equals(value, e.getValue()))
                    return true;
            }
            return false;
        }
    }
}
