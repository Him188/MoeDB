package net.mamoe.moedb.db;

import java.util.*;

/**
 * 键值式数据库
 *
 * @author Him188(code)      @ MoeDB Project
 * @since MoeDB 1.0.0
 */
@SuppressWarnings({"unused", "SameParameterValue"})
public interface KeyValueDatabase extends Database {
	/**
	 * @author Him188 @ MoeDB Project
	 * @see #listInsert(String, InsertPosition, Object, Object)
	 * @since MoeDB 1.0.0
	 */
	enum InsertPosition {
		LEFT,
		RIGHT
	}

	/**
	 * 获取所有的 key(包括字符串, 哈希表, 集合)
	 *
	 * @return 所有的 key
	 */
	Collection<String> getKeys();


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
	LinkedList<Object> multiGet(String... keys);

	LinkedList<Object> multiGet(LinkedList<Object> defaultValue, String... keys);

	/* HashTable */

	/**
	 * 删除 key 中的 field
	 *
	 * @param key   key
	 * @param field field
	 */
	@SuppressWarnings("SameParameterValue")
	boolean hashDelete(String key, String field);

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
	Object hashGet(String key, String field);

	Object hashGet(String key, String field, Object defaultValue);

	/**
	 * 获取 key 中所有键和值
	 *
	 * @param key key
	 *
	 * @return all keys and values
	 */
	Map<String, Object> hashGetAll(String key);

	Map<String, Object> hashGetAll(String key, Map<String, Object> defaultValue);

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
	List<Object> hashMultiGet(String key, String... fields);

	List<Object> hashMultiGet(String key, List<Object> defaultValue, String... fields);

	/**
	 * 设置 key 整体的值为 value
	 *
	 * @param key   key
	 * @param value value
	 */
	boolean hashSet(String key, Map<String, Object> value);

	/**
	 * 设置 key 中 field 的值为 value
	 *
	 * @param key   key
	 * @param field field
	 * @param value value
	 */
	boolean hashSet(String key, String field, Object value);

	/**
	 * 只有在 key 不存在时才设置 key 的值为 value
	 *
	 * @param key   key
	 * @param field field
	 * @param value value
	 */
	boolean hashSetIfNx(String key, String field, Object value);

	/**
	 * 获取 key 的所有值
	 *
	 * @param key key
	 *
	 * @return value list
	 */
	List<Object> hashGetValues(String key);

	List<Object> hashGetValues(String key, List<Object> defaultValue);

	/* List */

	/**
	 * 获取 key 中第 index 个值
	 *
	 * @param key   key
	 * @param index index
	 *
	 * @return value
	 */
	Object listGet(String key, long index);

	/**
	 * 获取 key 中第 index 个值
	 *
	 * @param key   key
	 * @param index index
	 *
	 * @return value
	 */
	Object listGet(String key, long index, Object defaultValue);

	/**
	 * 在 key 中 existing_value 的前/后方插入 value.
	 *
	 * @param key            key
	 * @param position       false: before, true: after
	 * @param existing_value existing_value
	 * @param value          value
	 */
	boolean listInsert(String key, InsertPosition position, Object existing_value, Object value);

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
	 * @return the first value defaults null
	 */
	Object listLeftPop(String key);

	Object listLeftPop(String key, Object defaultValue);

	/**
	 * 删除并获取 key 的最后一个值
	 *
	 * @param key key
	 *
	 * @return the last value default null
	 */
	Object listRightPop(String key);

	Object listRightPop(String key, Object defaultValue);

	/**
	 * 将一个或多个值插入到 key 的头部. 若 key 不存在, 将创建一个 key
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listLeftPush(String key, Object... value);

	/**
	 * 将一个值或多个值插入到 key 的头部. 若 key 不存在, 将不进行任何操作
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listLeftPushIfNx(String key, Object... value);

	/**
	 * 将一个或多个值插入到 key 的尾部. 若 key 不存在, 将创建一个 key
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listRightPush(String key, Object... value);

	/**
	 * 将一个值或多个值插入到 key 的尾部. 若 key 不存在, 将不进行任何操作
	 *
	 * @param key   key
	 * @param value values
	 */
	boolean listRightPushIfNx(String key, Object... value);

	/**
	 * 批量获取 key 中从 start 到 end 的值.
	 *
	 * @param key   key
	 * @param start start position, starts by 0
	 * @param end   end position.
	 *
	 * @return values
	 */
	List<Object> listRange(String key, long start, long end);

	/**
	 * 批量获取 key 中从 start 到表尾的值.
	 *
	 * @param key   key
	 * @param start start position, starts by 0
	 *
	 * @return values
	 */
	@SuppressWarnings("SameParameterValue")
	List<Object> listRange(String key, long start);

	/**
	 * 获取 key 中所有值
	 *
	 * @param key key
	 *
	 * @return values
	 */
	List<Object> listGetAll(String key);

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
	long listRemove(String key, long count, Object value);

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
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, long, Object)
	 */
	long listDelete(String key, long count, Object value);

	/**
	 * 删除 key 中的 value
	 *
	 * @param key   key
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, Object)
	 */
	long listRemove(String key, Object value);

	/**
	 * 删除 key 中的 value
	 *
	 * @param key   key
	 * @param value value
	 *
	 * @return returns how many values it has deleted, if key is not exits, this method will return 0L.
	 *
	 * @see #listRemove(String, long, Object)
	 */
	long listDelete(String key, Object value);

	/**
	 * 通过索引设置 key 的值
	 *
	 * @param key   key
	 * @param index starts by 0
	 * @param value value
	 */
	boolean listSet(String key, long index, Object value);

	/**
	 * 对表进行修剪. 即让列表只保留指定区间内的值. 不在指定区间内的值都将被删除
	 *
	 * @param key   key
	 * @param start start
	 * @param end   end
	 */
	boolean listTrim(String key, long start, long end);
}
