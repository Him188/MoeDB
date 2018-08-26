package net.mamoe.moedb.defaults;

import net.mamoe.moedb.AbstractDatabase;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisShardInfo;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;

/**
 * Redis 数据库 <br>
 * <a href="https://baike.baidu.com/item/Redis/6549233?fr=aladdin">查看百度百科介绍</a> <br>
 * 本类不会对值类型进行转换, 值类型有误时将会抛出异常({@link IllegalArgumentException}) <br>
 * 若需自动转换, 请使用 {@link SafeRedisDatabase}
 *
 * @author Him188 @ MoeDB Project
 */
@SuppressWarnings({"WeakerAccess", "unused"})
public class RedisDatabase extends AbstractDatabase {
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

    public String select(int id) {
        return client.select(id);
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
        return !client.type(Objects.requireNonNull(key, "key").toString()).equals("none");
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
    public Object get(Object key) {
        if (key == null) {
            throw new NullPointerException("key");
        }

        String stringKey = String.valueOf(key);

        switch (client.type(stringKey)) {
            case "none":
                return null;
            case "map":
                return client.hgetAll(stringKey);
            case "set":
                return client.zrange(stringKey, 0, -1);
            case "list":
                return client.lrange(stringKey, 0, -1);
            case "string":
                return client.get(stringKey);
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
     * @throws ClassCastException       当参数 <code>value</code> 的类型无效时抛出(参数类型属于 Map, List 或 Set, 但值类型或键类型不是 String)
     * @throws IllegalArgumentException 当参数 <code>value</code> 的类型无效时抛出(参数类型不属于 String, Map, List 或 Set)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object put(String key, Object value) throws ClassCastException, IllegalArgumentException {
        if (key == null) {
            throw new NullPointerException("key");
        }

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

        throw new IllegalArgumentException("type of argument value must be String, Map, List or set");
    }

    @Override
    public Object remove(Object key) {
        Object value = this.get(Objects.requireNonNull(key, "key"));
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
        return client.keys("*");
    }

    @Override
    public Collection<Object> values() {
        Collection<Object> values = new ArrayList<>();

        Set<String> keys = this.keySet();
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
                    values.add(new SimpleEntry(this, key, client.hgetAll(key)));
                    continue;
                case "set":
                    values.add(new SimpleEntry(this, key, client.zrange(key, 0, -1)));
                    continue;
                case "list":
                    values.add(new SimpleEntry(this, key, client.lrange(key, 0, -1)));
                    continue;
                case "string":
                    values.add(new SimpleEntry(this, key, client.get(key)));
                    continue;
                default:

            }
        }

        return values;
    }

    @Override
    public ChildDatabase getChildDatabase(String key) {
        if (key == null) {
            throw new NullPointerException("key");
        }
        if (!client.type(key).equals("map")) {
            throw new IllegalArgumentException("type of key is not map");
        }
        return new ChildDatabase(this, key);
    }

    @Override
    public short getShort(String key, short defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Short.valueOf(value.toString());
    }

    @Override
    public int getInteger(String key, int defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Integer.valueOf(value.toString());
    }

    @Override
    public long getLong(String key, long defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Long.valueOf(value.toString());
    }

    @Override
    public float getFloat(String key, float defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Float.valueOf(value.toString());
    }

    @Override
    public double getDouble(String key, double defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Double.valueOf(value.toString());
    }

    @Override
    public byte getByte(String key, byte defaultValue) {
        Object value = this.get(key);
        return value == null ? defaultValue : Byte.valueOf(value.toString());
    }

    @Override
    public char getCharacter(String key, char defaultValue) {
        Object value = this.get(key);
        return value == null || value.toString().length() == 0 ? defaultValue : value.toString().charAt(0);
    }

    @Override
    public List<String> getList(String key) {
        return client.lrange(key, 0, -1);
    }

    @Override
    public Map<String, String> getMap(String key) {
        return client.hgetAll(key);
    }

    @Override
    public Map<String, ?> getRawMap(String key) {
        return getMap(key);
    }

    @Override
    public void forEach(BiConsumer<? super String, ? super Object> action) {
        for (String s : this.keySet()) {
            action.accept(s, get(s));
        }
    }

    public static class ChildDatabase extends AbstractDatabase {
        public static final String NAME = "RedisChild";

        protected final RedisDatabase database;
        protected final String parentKey;

        protected ChildDatabase(RedisDatabase parentDatabase, String parentKey) {
            super();
            database = parentDatabase;
            this.parentKey = parentKey;
        }

        @Override
        public String getName() {
            return NAME;
        }

        @Override
        public String get(Object key) {
            if (key == null) {
                throw new NullPointerException("key");
            }
            return database.getClient().hget(parentKey, String.valueOf(key));
        }

        @Override
        public String put(String key, Object value) {
            if (key == null) {
                throw new NullPointerException("key");
            }
            if (value == null) {
                throw new NullPointerException("value");
            }
            String old = get(key);
            database.getClient().hset(parentKey, key, String.valueOf(value));
            return old;
        }

        @Override
        public void clear() {
            database.getClient().del(parentKey);
        }

        @SuppressWarnings("unchecked")
        @Override
        public void putAll(Map<? extends String, ?> m) {
            database.getClient().del(parentKey);
            database.getClient().hmset(parentKey, (Map<String, String>) m);
        }

        @Override
        public int size() {
            return (int) (long) database.getClient().hlen(parentKey);
        }

        @Override
        public AbstractDatabase getChildDatabase(String key) {
            throw new UnsupportedOperationException("redis child database has no child");
        }
    }

    private static final class SimpleEntry implements Entry<String, Object> {
        private final RedisDatabase database;
        private final String key;
        private Object value;

        public SimpleEntry(RedisDatabase parentDatabase, String key, Object value) {
            database = parentDatabase;
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
            return database.put(key, this.value = value);
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
