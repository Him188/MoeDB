package net.mamoe.moedb.db;

import redis.clients.jedis.*;
import redis.clients.jedis.exceptions.JedisConnectionException;

import java.util.*;

/**
 * Redis 数据库. 本数据库无需手动保存, 所有保存工作交给 Redis 服务器来进行
 *
 *
 * 以下是摘自 "百度百科" 的 Redis 介绍
 *
 * <p>
 * redis 是一个 key-value 存储系统. 和 Memcached 类似, 它支持存储的 value 类型相对更多, 包括 string(字符串), list(链表), set(集合),
 * zset(sorted set --有序集合)和 hash (哈希类型). 这些数据类型都支持 push/pop, add/remove 及取交集并集和差集及更丰富的操作, 而且这些操作都是原子性的.
 * 在此基础上,redis支持各种不同方式的排序.与 memcached 一样, 为了保证效率, 数据都是缓存在内存中.
 * 区别的是 redis 会周期性的把更新的数据写入磁盘或者把修改操作写入追加的记录文件, 并且在此基础上实现了 master-slave(主从) 同步.
 *
 * Redis 是一个高性能的 key-value 数据库. redis 的出现, 很大程度补偿了 memcached 这类 key/value 存储的不足, 在部分场合可以对关系数据库起到很好的补充作用.
 * 它提供了Java,C/C++, C#, PHP, JavaScript, Perl, Object-C, Python, Ruby, Erlang 等客户端, 使用很方便. [1] Redis支持主从同步.
 * 数据可以从主服务器向任意数量的从服务器上同步, 从服务器可以是关联其他从服务器的主服务器. 这使得 Redis 可执行单层树复制.
 * 存盘可以有意无意的对数据进行写操作. 由于完全实现了发布/订阅机制, 使得从数据库在任何地方同步树时, 可订阅一个频道并接收主服务器完整的消息发布记录.
 * 同步对读取操作的可扩展性和数据冗余很有帮助. redis 的官网地址, 非常好记, 是 redis.io.  (特意查了一下, 域名后缀 io 属于国家域名,
 * 是 british Indian Ocean territory, 即英属印度洋领地)  目前, Vmware 在资助着 redis 项目的开发和维护.
 * </p>
 *
 *
 * 值得注意的是, Redis 保存的所有数据都是字符串, 因此, 使用 get 类型的方法读取出来的数据都是 String 请勿直接进行强转,
 * 本类 list 开头的方法使用的均是 Redis 的 list(链表), 而不是 set(集合). 集合暂未支持, 你可以通过 {@link #getClient()} 自行操作 set.
 * 可使用 {@link Integer#parseInt(String)} 等方法
 *
 * @since MoeDB 1.0.0
 */
@SuppressWarnings({"SameParameterValue", "unused", "WeakerAccess"})
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

	public Jedis getClient() {
		return client;
	}

	@Override
	public Collection<String> getKeys() {
		return client.keys("(.*)");//matches all
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
			client.linsert(key, InsertPosition.LEFT == position ? BinaryClient.LIST_POSITION.AFTER :
					BinaryClient.LIST_POSITION.BEFORE, existing_value.toString(), value.toString());
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
