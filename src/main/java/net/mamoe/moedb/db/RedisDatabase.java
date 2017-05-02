package net.mamoe.moedb.db;

import redis.clients.jedis.*;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.*;

/**
 * @author Him188 @ MoeDB Project
 * @since MoeDB 1.0.0
 */
@SuppressWarnings("SameParameterValue")
public class RedisDatabase implements KeyValueDatabase {
	private Jedis client = null;

	public RedisDatabase(String host, int port, String user, String password) throws JedisConnectionException {
		init(host, port, user, password);
	}

	@Override
	public String getName() {
		return "Redis";
	}

	/**
	 * 初始化(连接)数据库
	 *
	 * @param host     Redis server IP
	 * @param port     Redis server port
	 * @param user     username
	 * @param password password
	 */
	@SuppressWarnings("SameParameterValue")
	private void init(String host, int port, String user, String password) throws JedisConnectionException {
		JedisShardInfo info = new JedisShardInfo(host, port, user);
		info.setPassword(password);
		client = new Jedis(info);
		client.connect();
	}

	/**
	 * 切换到数据库
	 *
	 * @param id 数据库 id
	 */
	public synchronized boolean select(int id) {
		try {
			client.select(id);
			return true;
		} catch (Exception ignored) {
			return false;
		}
	}

	/**
	 * 检查连接是否正常
	 */
	public synchronized boolean ping() {
		return client.ping().equals("PONG");
	}

	/**
	 * 使 Redis 服务端控制台打印一条消息.
	 *
	 * @param message message
	 */
	public synchronized boolean echo(String message) {
		try {
			client.echo(message);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/* *********************************************** */
	/* ******************** 字符串 ******************** */
	/* *********************************************** */

	@Override
	public synchronized Object get(String key) {
		return get(key, null);
	}

	@Override
	public synchronized Object get(String key, Object defaultValue) {
		try {
			String got = client.get(key);
			return got == null ? defaultValue : got;
		} catch (Exception e) {
			return defaultValue;
		}
	}

	@Override
	public synchronized boolean set(String key, Object value) {
		try {
			client.set(key, value.toString());
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized Object getSet(String key, Object value) {
		return getSet(key, value, null);
	}

	@Override
	public synchronized Object getSet(String key, Object value, Object defaultValue) {
		try {
			return client.getSet(key, value.toString());
		} catch (Exception e) {
			return defaultValue;
		}
	}

	@Override
	public synchronized LinkedList<Object> multiGet(String... keys) {
		return multiGet(new LinkedList<>(), keys);
	}

	@Override
	public synchronized LinkedList<Object> multiGet(LinkedList<Object> defaultValue, String... keys) {
		try {
			return new LinkedList<Object>() {{
				addAll(client.mget(keys));
			}};
		} catch (Exception e) {
			return defaultValue;
		}
	}

	public synchronized boolean multiSet(String... keys_values) {
		try {
			client.mset(keys_values);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 将 key 设置为 value, 并指定该值的过期时间.
	 *
	 * @param key     key
	 * @param value   value
	 * @param seconds expireTime
	 */
	public synchronized boolean setExpired(String key, String value, int seconds) {
		try {
			client.setex(key, seconds, value);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 在 key 不存在时设置为 value. 若已存在, 则不进行任何操作
	 *
	 * @param key   key
	 * @param value value
	 */
	public synchronized boolean setIfNx(String key, String value) {
		try {
			client.setnx(key, value);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 在所有给定的 key 都不存在时设置为 value. 若有一个 key 存在, 则不进行任何操作
	 * 用法: object.MSet(key1, value1, key2, value2, key3, value3);
	 *
	 * @param keys_values key1 value1 key2 value2 key3 value3
	 */
	public synchronized boolean multiSetNx(String... keys_values) {
		try {
			client.msetnx(keys_values);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 获取 key 的值的长度
	 *
	 * @param key key
	 *
	 * @return length default 0
	 */
	public synchronized long getLength(String key) {
		return getLength(key, 0);
	}

	public synchronized long getLength(String key, long defaultValue) {
		try {
			return client.strlen(key);
		} catch (Exception e) {
			return defaultValue;
		}
	}

	/**
	 * 使该 key 的值 +1
	 *
	 * @param key key
	 */
	public synchronized boolean increase(String key) {
		try {
			client.incr(key);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 使该 key 的值 +amount
	 *
	 * @param key    key
	 * @param amount amount
	 */
	public synchronized boolean increaseBy(String key, long amount) {
		try {
			client.incrBy(key, amount);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 使该 key 的值 -1
	 *
	 * @param key key
	 */
	public synchronized boolean decrease(String key) {
		try {
			client.decr(key);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 使该 key 的值 -amount
	 *
	 * @param key    key
	 * @param amount amount
	 */
	public synchronized boolean decreaseBy(String key, long amount) {
		try {
			client.decrBy(key, amount);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 如果 key 已经存在并且是一个字符串, 则将 value 追加到 key 的值末尾
	 *
	 * @param key   key
	 * @param value value
	 */
	public synchronized boolean append(String key, String value) {
		try {
			client.append(key, value);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/* *********************************************** */
	/* *********************************************** */


	/* *********************************************** */
	/* ******************** 哈希表 ******************** */
	/* *********************************************** */

	/**
	 * 删除 key 中的 fields
	 *
	 * @param key    key
	 * @param fields fields
	 */
	@Override
	@SuppressWarnings("SameParameterValue")
	public synchronized boolean hashDelete(String key, String fields) {
		try {
			client.hdel(key, fields);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 检查 key 中是否存在 field
	 *
	 * @param key   key
	 * @param field field
	 *
	 * @return if exits
	 */
	@Override
	public synchronized boolean hashExits(String key, String field) {
		try {
			return client.hexists(key, field);
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 获取 key 中 field 的值
	 *
	 * @param key   key
	 * @param field field
	 *
	 * @return value
	 */
	@Override
	public synchronized Object hashGet(String key, String field) {
		return hashGet(key, field, "");
	}

	@Override
	public synchronized Object hashGet(String key, String field, Object defaultValue) {
		String value;
		try {
			value = client.hget(key, field);
		} catch (Exception ex) {
			return defaultValue;
		}

		return value == null || value.equals("nil") ? defaultValue : value;
	}

	/**
	 * 获取 key 中所有键和值
	 *
	 * @param key key
	 *
	 * @return all keys and values
	 */
	@Override
	public synchronized Map<String, Object> hashGetAll(String key) {
		return hashGetAll(key, new HashMap<>());
	}

	@Override
	public synchronized Map<String, Object> hashGetAll(String key, Map<String, Object> defaultValue) {
		try {
			return new LinkedHashMap<>(client.hgetAll(key));
		} catch (Exception e) {
			return defaultValue;
		}
	}

	/**
	 * 获取 key 中键数量
	 *
	 * @param key key
	 *
	 * @return length
	 */
	@Override
	public synchronized long hashGetLength(String key) {
		return hashGetLength(key, 0);
	}

	@Override
	public synchronized long hashGetLength(String key, long defaultValue) {
		try {
			return client.hlen(key);
		} catch (Exception e) {
			return defaultValue;
		}
	}

	/**
	 * 批量获取 key 中 fields 的值
	 *
	 * @param key    key
	 * @param fields fields
	 *
	 * @return values
	 */
	@Override
	public synchronized List<Object> hashMultiGet(String key, String... fields) {
		return hashMultiGet(key, new ArrayList<>(), fields);

	}

	@Override
	public synchronized List<Object> hashMultiGet(String key, List<Object> defaultValue, String... fields) {
		try {
			return new LinkedList<>(client.hmget(key, fields));
		} catch (Exception e) {
			return defaultValue;
		}

	}

	/**
	 * 设置 key 的值为 value
	 *
	 * @param key   key
	 * @param value value
	 */
	@Override
	public synchronized boolean hashSet(String key, Map<String, Object> value) {
		//client.hmset(key, value); // causes a fucking exception

		try {
			Map<String, String> original = client.hgetAll(key);
			original.putAll(new LinkedHashMap<String, String>() {
				{
					value.forEach((key, value) -> put(key, value.toString()));
				}
			});
			for (String s : array_change(original.keySet().toArray())) {
				client.hdel(key, s);
			}
			value.forEach((k, v) -> client.hset(key, k, v.toString()));
			return true;
		} catch (Exception e) {
			return false;
		}

	}

	private static String[] array_change(Object[] array) {
		String[] result = new String[array.length];
		for (int i = 0; i < array.length; i++) {
			result[i] = array[i].toString();
		}
		return result;
	}

	@Override
	public synchronized boolean hashSet(String key, String field, Object value) {
		if (value == null) {
			value = "";
		}
		if (field == null) {
			return false;
		}

		//System.out.println(key + "||" + field + "||" + value);
		try {
			client.hset(key, field, value.toString());
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized boolean hashSetIfNx(String key, String field, Object value) {
		try {
			client.hsetnx(key, field, value.toString());
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized List<Object> hashGetValues(String key) {
		return hashGetValues(key, new ArrayList<>());
	}

	@Override
	public synchronized List<Object> hashGetValues(String key, List<Object> defaultValue) {
		try {
			return new LinkedList<>(client.hvals(key));
		} catch (Exception e) {
			return defaultValue;
		}
	}

	/* List */

	/**
	 * 批量删除并获取列表的第一个值. 如果该列表不存在值, 将会阻塞线程直到有可获取的值为止.
	 *
	 * @param timeout seconds
	 * @param keys    keys
	 *
	 * @return values
	 */
	public synchronized List<String> listLeftPop(int timeout, String... keys) {
		try {
			return client.blpop(timeout, keys);
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	/**
	 * 批量删除并获取列表的最后一个值. 如果该列表不存在值, 将会阻塞线程直到有可获取的值为止.
	 *
	 * @param timeout seconds
	 * @param keys    keys
	 *
	 * @return values
	 */
	public synchronized List<String> listRightPop(int timeout, String... keys) {
		try {
			return client.brpop(timeout, keys);
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public synchronized Object listGet(String key, long index) {
		return listGet(key, index, "");
	}

	@Override
	public synchronized Object listGet(String key, long index, Object defaultValue) {
		try {
			Object result = client.lindex(key, index);
			return result == null ? defaultValue : result;
		} catch (Exception e) {
			return defaultValue;
		}

	}

	@Override
	public synchronized boolean listInsert(String key, InsertPosition position, Object existing_value, Object value) {
		try {
			client.linsert(key, InsertPosition.LEFT == position ? BinaryClient.LIST_POSITION.AFTER : BinaryClient.LIST_POSITION.BEFORE, existing_value.toString(), value.toString());
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized long listLength(String key) {
		return listLength(key, 0);
	}

	@Override
	public synchronized long listLength(String key, long defaultValue) {
		try {
			return client.llen(key);
		} catch (Exception e) {
			return defaultValue;
		}
	}

	@Override
	public synchronized Object listLeftPop(String key) {
		return listLeftPop(key, null);
	}

	@Override
	public synchronized Object listLeftPop(String key, Object defaultValue) {
		String result;
		try {
			result = client.lpop(key);
			return result == null || result.equals("nil") ? result : defaultValue;
		} catch (Exception e) {
			return defaultValue;
		}
	}

	@Override
	public synchronized Object listRightPop(String key) {
		return listRightPop(key, null);
	}

	@Override
	public synchronized Object listRightPop(String key, Object defaultValue) {
		String result;
		try {
			result = client.rpop(key);
			return result == null || result.equals("nil") ? result : defaultValue;
		} catch (Exception e) {
			return defaultValue;
		}
	}

	@Override
	public synchronized boolean listLeftPush(String key, Object... value) {
		try {
			client.lpush(key, cast(value));
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	private static String[] cast(Object[] value) {
		String[] result = new String[value.length];
		for (int i = 0; i < value.length; i++) {
			result[i] = value[i].toString();
		}

		return result;
	}

	@Override
	public synchronized boolean listLeftPushIfNx(String key, Object... value) {
		try {
			for (Object s : value) {
				client.lpushx(key, s.toString());
			}
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized boolean listRightPush(String key, Object... value) {
		try {
			client.rpush(key, cast(value));

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized boolean listRightPushIfNx(String key, Object... value) {
		try {
			for (Object s : value) {
				client.rpushx(key, s.toString());
			}
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized List<Object> listRange(String key, long start, long end) {
		try {
			return new LinkedList<>(client.lrange(key, start, end));
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@SuppressWarnings("SameParameterValue")
	@Override
	public synchronized List<Object> listRange(String key, long start) {
		try {
			return new LinkedList<>(client.lrange(key, start, -1));
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public synchronized List<Object> listGetAll(String key) {
		return listRange(key, 0);
	}

	@Override
	public synchronized long listRemove(String key, long count, Object value) {
		try {
			return client.lrem(key, count, value.toString());
		} catch (Exception e) {
			return 0L;
		}
	}

	@Override
	public synchronized long listDelete(String key, long count, Object value) {
		return listRemove(key, count, value);
	}

	@Override
	public synchronized long listRemove(String key, Object value) {
		return listRemove(key, 0, value);
	}

	@Override
	public synchronized long listDelete(String key, Object value) {
		return listRemove(key, 0, value);
	}

	@Override
	public synchronized boolean listSet(String key, long index, Object value) {
		try {
			client.lset(key, index, value.toString());
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public synchronized boolean listTrim(String key, long start, long end) {
		try {
			client.ltrim(key, start, end);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/* *********************************************** */

	/**
	 * 订阅一个或多个频道
	 *
	 * @param listener event handler
	 * @param channels channels
	 */
	@SuppressWarnings("SpellCheckingInspection")
	public synchronized boolean subscribe(JedisPubSub listener, String... channels) {
		try {
			client.subscribe(listener, channels);

			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 订阅一个或多个模式的频道
	 * 每个模式以 * 作为匹配符
	 * 比如 it* 匹配所有以 it 开头的频道("it.news", "it.blog", "it.tweets", etc.)
	 * news.* 匹配所有以 news. 开头的频道("news.it", "news.global.today", etc.)
	 *
	 * @param listener event handler
	 * @param channels channels
	 */
	@SuppressWarnings("SpellCheckingInspection")
	public synchronized boolean psubscribe(JedisPubSub listener, String... channels) {
		try {
			client.psubscribe(listener, channels);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 向一个频道发送信息
	 *
	 * @param channel channel
	 * @param message message
	 */
	public synchronized boolean publish(String channel, String message) {
		try {
			client.publish(channel, message);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/* *********************************************** */

	/**
	 * 标记事务开始.
	 */
	public synchronized Transaction multi() {
		try {
			return client.multi();
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * 监视一个或多个 key, 如果在事务执行前这些 key 被改动, 将停止事务的执行
	 *
	 * @param keys keys
	 */
	public synchronized boolean watch(String... keys) {
		try {
			client.watch(keys);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * 取消监视 key
	 */
	public synchronized boolean unwatch() {
		try {
			client.unwatch();
			return true;
		} catch (Exception e) {
			return false;
		}
	}
}
