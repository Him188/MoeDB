package net.mamoe.moedb.db;

import java.util.List;
import java.util.Map;

/**
 * 键值式数据库
 *
 * @author Him188(code)      @ MoeDB Project
 * @since MoeDB 1.0.0
 */
@SuppressWarnings({"unused", "SameParameterValue"})
interface KeyValueDatabase extends Database {
	/* String */

	/**
	 * 获取 {@code key} 的值.
	 *
	 * @param key key
	 *
	 * @return value
	 */
	Object get(String key);

	/**
	 * 获取 key 的值, 若 key 的值为空, 则返回 defaultValue
	 *
	 * @param key          key
	 * @param defaultValue defaultValue
	 *
	 * @return value|defaultValue
	 */
	Object get(String key, Object defaultValue);

	boolean set(String key, Object value);

	/**
	 * 将 key 设置为 value 并返回该 key 以前的值
	 *
	 * @param key   key
	 * @param value new value
	 *
	 * @return old value
	 */
	Object getSet(String key, Object value);

	Object getSet(String key, Object value, Object defaultValue);

	/**
	 * 批量获取值
	 *
	 * @param keys keys
	 *
	 * @return value list
	 */
	List<String> multiGet(String... keys);

	List<String> multiGet(List<String> defaultValue, String... keys);

	/**
	 * 批量设置 key 和 value
	 * 用法: object.multiSet(key1, value1, key2, value2, key3, value3);
	 *
	 * @param keys_values key1 value1 key2 value2 key3 value3
	 */
	boolean multiSet(String... keys_values);

	/* HashTable */

	/**
	 * 删除 key 中的 fields
	 *
	 * @param key    key
	 * @param fields fields
	 */
	@SuppressWarnings("SameParameterValue")
	boolean hashDelete(String key, String fields);

	/**
	 * 检查 key 中是否存在 field
	 *
	 * @param key   key
	 * @param field field
	 *
	 * @return if exits
	 */
	boolean hashExits(String key, String field);

	/**
	 * 获取 key 中 field 的值
	 *
	 * @param key   key
	 * @param field field
	 *
	 * @return value
	 */
	String hashGet(String key, String field);

	String hashGet(String key, String field, String defaultValue);

	/**
	 * 获取 key 中所有键和值
	 *
	 * @param key key
	 *
	 * @return all keys and values
	 */
	Map<String, String> hashGetAll(String key);

	Map<String, String> hashGetAll(String key, Map<String, String> defaultValue);

	/**
	 * 获取 key 中键数量
	 *
	 * @param key key
	 *
	 * @return length
	 */
	long hashGetLength(String key);

	long hashGetLength(String key, long defaultValue);

	/**
	 * 批量获取 key 中 fields 的值
	 *
	 * @param key    key
	 * @param fields fields
	 *
	 * @return values
	 */
	List<String> hashMultiGet(String key, String... fields);

	List<String> hashMultiGet(String key, List<String> defaultValue, String... fields);

	/**
	 * 设置 key 的值为 value
	 *
	 * @param key   key
	 * @param value value
	 */
	boolean hashSet(String key, Map<String, String> value);

	/**
	 * 设置 key 的值为 value
	 *
	 * @param key   key
	 * @param field field
	 * @param value value
	 */
	boolean hashSet(String key, String field, String value);

	/**
	 * 只有在 key 不存在时才设置 key 的值为 value
	 *
	 * @param key   key
	 * @param field field
	 * @param value value
	 */
	boolean hashSetIfNx(String key, String field, String value);

	/**
	 * 获取 key 的所有值
	 *
	 * @param key key
	 *
	 * @return value list
	 */
	List<String> hashGetValues(String key);

	List<String> hashGetValues(String key, List<String> defaultValue);

	/* List */

	/**
	 * 批量删除并获取列表的第一个值. 如果该列表不存在值, 将会阻塞线程直到有可获取的值为止.
	 *
	 * @param timeout seconds
	 * @param keys    keys
	 *
	 * @return values
	 */
	List<String> listLeftPop(int timeout, String... keys);

	/**
	 * 批量删除并获取列表的最后一个值. 如果该列表不存在值, 将会阻塞线程直到有可获取的值为止.
	 *
	 * @param timeout seconds
	 * @param keys    keys
	 *
	 * @return values
	 */
	List<String> listRightPop(int timeout, String... keys);

	/**
	 * 获取 key 中第 index 个值
	 *
	 * @param key   key
	 * @param index index
	 *
	 * @return value
	 */
	String listGet(String key, long index);

	/**
	 * 获取 key 中第 index 个值
	 *
	 * @param key   key
	 * @param index index
	 *
	 * @return value
	 */
	String listGet(String key, long index, String defaultValue);

	/**
	 * 在 key 中 existing_value 的前/后方插入 value.
	 *
	 * @param key            key
	 * @param position       false: before, true: after
	 * @param existing_value existing_value
	 * @param value          value
	 */
	boolean listInsert(String key, boolean position, String existing_value, String value);

	/**
	 * 获取 key 的长度
	 *
	 * @param key key
	 *
	 * @return length default 0
	 */
	long listLength(String key);

	long listLength(String key, long defaultValue);

	/**
	 * 删除并获取 key 的第一个值
	 *
	 * @param key key
	 *
	 * @return the first value default null
	 */
	String listLeftPop(String key);

	String listLeftPop(String key, String defaultValue);

	/**
	 * 删除并获取 key 的最后一个值
	 *
	 * @param key key
	 *
	 * @return the last value default null
	 */
	String listRightPop(String key);

	String listRightPop(String key, String defaultValue);

	/**
	 * 将一个或多个值插入到 key 的头部. 若 key 不存在, 将创建一个 key
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listLeftPush(String key, String... value);

	/**
	 * 将一个值或多个值插入到 key 的头部. 若 key 不存在, 将不进行任何操作
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listLeftPushIfNx(String key, String... value);

	/**
	 * 将一个或多个值插入到 key 的尾部. 若 key 不存在, 将创建一个 key
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listRightPush(String key, String... value);

	/**
	 * 将一个值或多个值插入到 key 的尾部. 若 key 不存在, 将不进行任何操作
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listRightPushIfNx(String key, String... value);

	/**
	 * 批量获取 key 中从 start 到 end 的值.
	 *
	 * @param key   key
	 * @param start start position, starts by 0
	 * @param end   end position.
	 *
	 * @return values
	 */
	List<String> listRange(String key, long start, long end);

	/**
	 * 批量获取 key 中从 start 到表尾的值.
	 *
	 * @param key   key
	 * @param start start position, starts by 0
	 *
	 * @return values
	 */
	@SuppressWarnings("SameParameterValue")
	List<String> listRange(String key, long start);

	/**
	 * 获取 key 中所有值
	 *
	 * @param key key
	 *
	 * @return values
	 */
	List<String> listGetAll(String key);

	/**
	 * 删除 key 中的 value
	 * count:
	 * count > 0 : 从表头开始向表尾搜索，移除与 value 相等的值，数量为 count 。
	 * count < 0 : 从表尾开始向表头搜索，移除与 value 相等的值，数量为 count 的绝对值。
	 * count = 0 : 移除表中所有与 value 相等的值
	 *
	 * @param key   key
	 * @param count count
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exists, this method will return 0L.
	 */
	long listRemove(String key, long count, String value);

	/**
	 * 同 #listRemove(String, long, String)
	 * <p>
	 * 删除 key 中的 value
	 * count:
	 * count > 0 : 从表头开始向表尾搜索，移除与 value 相等的值，数量为 count 。
	 * count < 0 : 从表尾开始向表头搜索，移除与 value 相等的值，数量为 count 的绝对值。
	 * count = 0 : 移除表中所有与 value 相等的值
	 *
	 * @param key   key
	 * @param count count
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, long, String)
	 */
	long listDelete(String key, long count, String value);

	/**
	 * 同 #listDelete(String, long, String)
	 * <p>
	 * 删除 key 中的 value
	 *
	 * @param key   key
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, long, String)
	 */
	long listRemove(String key, String value);

	/**
	 * 同 #listRemove(String, long, String)
	 * <p>
	 * 删除 key 中的 value
	 *
	 * @param key   key
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, long, String)
	 */
	long listDelete(String key, String value);

	/**
	 * 通过索引设置 key 的值
	 *
	 * @param key   key
	 * @param index starts by 0
	 * @param value value
	 */
	boolean listSet(String key, long index, String value);

	/**
	 * 对表进行修剪. 即让列表只保留指定区间内的值. 不在指定区间内的值都将被删除
	 *
	 * @param key   key
	 * @param start start
	 * @param end   end
	 */
	boolean listTrim(String key, long start, long end);
}
