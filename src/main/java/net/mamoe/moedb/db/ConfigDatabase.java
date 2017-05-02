package net.mamoe.moedb.db;

import cn.nukkit.utils.Config;

import java.util.*;

import static net.mamoe.moedb.Utils.*;

/**
 * @author Him188 @ MoeDB Project
 * @since MoeDB 1.0.0
 */
public class ConfigDatabase implements KeyValueDatabase {
	private final ChildConfig strings;
	private final ChildConfig maps;
	private final ChildConfig lists;

	public ConfigDatabase(Config config) {
		this.strings = new ChildConfig("strings", config);
		this.maps = new ChildConfig("maps", config);
		this.lists = new ChildConfig("lists", config);
	}

	@Override
	public String getName() {
		return "Config";
	}

	@Override
	public Object get(String key) {
		return strings.get(key);
	}

	@Override
	public Object get(String key, Object defaultValue) {
		return strings.getOrDefault(key, defaultValue);
	}

	@Override
	public boolean set(String key, Object value) {
		strings.set(key, value);
		return true;
	}

	@Override
	public Object getSet(String key, Object value) {
		Object original = strings.get(key);
		strings.set(key, value);
		return original;
	}

	@Override
	public Object getSet(String key, Object value, Object defaultValue) {
		Object original = strings.get(key);
		strings.set(key, value);
		return original == null ? defaultValue : original;
	}

	@Override
	public LinkedList<Object> multiGet(String... keys) {
		return new LinkedList<Object>() {
			{
				for (String key : keys) {
					add(strings.get(key));
				}
			}
		};
	}

	@Override
	public LinkedList<Object> multiGet(LinkedList<Object> defaultValue, String... keys) {
		LinkedList<Object> list = new LinkedList<Object>() {
			{
				for (String key : keys) {
					add(strings.get(key));
				}
			}
		};

		return list.isEmpty() ? defaultValue : list;
	}

	/* HashMap */

	@SuppressWarnings("unchecked")
	private LinkedHashMap<String, Object> getMap(String key) {
		return (LinkedHashMap<String, Object>) maps.getOrDefault(key, new LinkedHashMap<String, Object>());
	}

	@Override
	public boolean hashDelete(String key, String field) {
		LinkedHashMap<String, Object> map = getMap(key);
		map.remove(field);
		maps.set(key, map);
		return true;
	}

	@Override
	public boolean hashExits(String key, String field) {
		return getMap(key).get(field) != null;
	}

	@Override
	public Object hashGet(String key, String field) {
		return getMap(key).get(field);
	}

	@Override
	public Object hashGet(String key, String field, Object defaultValue) {
		return getMap(key).getOrDefault(field, defaultValue);
	}

	@Override
	public LinkedHashMap<String, Object> hashGetAll(String key) {
		return getMap(key);
	}

	@Override
	public LinkedHashMap<String, Object> hashGetAll(String key, Map<String, Object> defaultValue) {
		return defaults(getMap(key), new LinkedHashMap<>(defaultValue));
	}

	@Override
	public long hashGetLength(String key) {
		return getMap(key).size();
	}

	@Override
	public long hashGetLength(String key, long defaultValue) {
		return defaults((long) getMap(key).size(), defaultValue);
	}

	@Override
	public LinkedList<Object> hashMultiGet(String key, String... fields) {
		return new LinkedList<Object>() {
			{
				LinkedHashMap<String, Object> map = getMap(key);
				for (String field : fields) {
					add(map.get(field));
				}
			}
		};
	}

	@Override
	public LinkedList<Object> hashMultiGet(String key, List<Object> defaultValue, String... fields) {
		return defaults(hashMultiGet(key, fields), new LinkedList<>(defaultValue));
	}

	@Override
	public boolean hashSet(String key, Map<String, Object> value) {
		LinkedHashMap<String, Object> map = getMap(key);
		map.clear();
		map.putAll(value);
		maps.set(key, map);
		return true;
	}

	@Override
	public boolean hashSet(String key, String field, Object value) {
		LinkedHashMap<String, Object> map = getMap(key);
		map.put(field, value);
		maps.set(key, map);
		return true;
	}

	@Override
	public boolean hashSetIfNx(String key, String field, Object value) {
		LinkedHashMap<String, Object> map = getMap(key);
		map.putIfAbsent(field, value);
		maps.set(key, map);
		return true;
	}

	@Override
	public LinkedList<Object> hashGetValues(String key) {
		return new LinkedList<>(getMap(key).values());
	}

	@Override
	public LinkedList<Object> hashGetValues(String key, List<Object> defaultValue) {
		return defaults(new LinkedList<>(getMap(key).values()), new LinkedList<>(defaultValue));
	}

	/* List */

	@SuppressWarnings("unchecked")
	private LinkedList<Object> getList(String key) {
		return (LinkedList<Object>) lists.getOrDefault(key, new LinkedList<>());
	}

	@Override
	public Object listGet(String key, long index) {
		try {
			return getList(key).get((int) index);
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Object listGet(String key, long index, Object defaultValue) {
		return defaults(listGet(key, index), defaultValue);
	}

	@Override
	public boolean listInsert(String key, InsertPosition position, Object existing_value, Object value) {
		LinkedList<Object> list = getList(key);
		int pos = listFind(list, existing_value);
		if (pos == -1) {
			return false;
		}

		if (position == InsertPosition.LEFT) {
			list.add(pos, value);
		} else {
			list.add(pos + 1, value);
		}
		lists.set(key, list);
		return true;
	}

	@Override
	public long listLength(String key) {
		return getList(key).size();
	}

	@Override
	public long listLength(String key, long defaultValue) {
		return defaults(getList(key).size(), defaultValue);
	}

	@Override
	public Object listLeftPop(String key) {
		try {
			LinkedList<Object> list = getList(key);
			Object result = list.removeFirst();
			lists.set(key, list);
			return result;
		} catch (NoSuchElementException e) {
			return null;
		}
	}

	@Override
	public Object listLeftPop(String key, Object defaultValue) {
		try {
			LinkedList<Object> list = getList(key);
			Object result = list.removeFirst();
			lists.set(key, list);
			return result;
		} catch (NoSuchElementException e) {
			return defaultValue;
		}
	}

	@Override
	public Object listRightPop(String key) {
		try {
			LinkedList<Object> list = getList(key);
			Object result = list.removeLast();
			lists.set(key, list);
			return result;
		} catch (NoSuchElementException e) {
			return null;
		}
	}

	@Override
	public Object listRightPop(String key, Object defaultValue) {
		try {
			LinkedList<Object> list = getList(key);
			Object result = list.removeLast();
			lists.set(key, list);
			return result;
		} catch (NoSuchElementException e) {
			return defaultValue;
		}
	}

	@Override
	public boolean listLeftPush(String key, Object... value) {
		value = arrayReverse(value);
		LinkedList<Object> list = getList(key);
		for (Object o : value) {
			list.addFirst(o);
		}
		return true;
	}

	@Override
	public boolean listLeftPushIfNx(String key, Object... value) {
		LinkedList<Object> list = getList(key);
		for (Object o : value) {
			if (list.contains(o)) {
				return false;
			}
		}

		return listLeftPush(key, value);
	}

	@Override
	public boolean listRightPush(String key, Object... value) {
		LinkedList<Object> list = getList(key);
		for (Object o : value) {
			list.addLast(o);
		}
		return true;
	}

	@Override
	public boolean listRightPushIfNx(String key, Object... value) {
		LinkedList<Object> list = getList(key);
		for (Object o : value) {
			if (list.contains(o)) {
				return false;
			}
		}

		return listRightPush(key, value);
	}

	@Override
	public LinkedList<Object> listRange(String key, long start, long end) {
		LinkedList<Object> list = getList(key);

		if (list.size() - 1 < end) {
			throw new IllegalArgumentException("Arg end must not be smaller than size of list");
		}

		if (end == -1) {
			end = list.size() - 1;
		}

		if (start < 0) {
			throw new IllegalArgumentException("Arg end must not be smaller than 0");
		}

		LinkedList<Object> result = new LinkedList<>();
		for (long i = start; i < end; i++) {
			result.add(list.get((int) i));
		}

		return result;
	}

	@Override
	public LinkedList<Object> listRange(String key, long start) {
		return listRange(key, start, -1);
	}

	@Override
	public List<Object> listGetAll(String key) {
		return getList(key);
	}

	@Override
	public long listRemove(String key, long count, Object value) {
		LinkedList<Object> list = getList(key);
		if (count == 0) {
			int originalSize = list.size();
			list.removeAll(new ArrayList<Object>() {
				{
					add(value);
				}
			});
			lists.set(key, list);
			return originalSize - list.size();
		} else if (count > 0) {
			int amount = 0;
			Iterator<?> it = list.iterator();
			while (it.hasNext()) {
				if (value.equals(it.next())) {
					it.remove();
					amount++;
				}
			}
			lists.set(key, list);
			return amount;
		} else if (count < 0) {
			int amount = 0;
			ListIterator<?> it = list.listIterator();
			while (it.hasNext()) {
				it.next();
			} //使迭代器位置到末尾
			while (it.hasPrevious()) {//倒序迭代
				if (value.equals(it.previous())) {
					it.remove();
					amount++;
				}
			}
			lists.set(key, list);
			return amount;
		}
		return 0;
	}

	@Override
	public long listDelete(String key, long count, Object value) {
		return listRemove(key, count, value);
	}

	@Override
	public long listRemove(String key, Object value) {
		return listRemove(key, 0, value);
	}

	@Override
	public long listDelete(String key, Object value) {
		return listRemove(key, value);
	}

	@Override
	public boolean listSet(String key, long index, Object value) {
		LinkedList<Object> list = getList(key);
		list.set((int) index, value);
		lists.set(key, list);
		return true;
	}

	@Override
	public boolean listTrim(String key, long start, long end) {
		try {
			LinkedList<Object> list = listRange(key, start, end);
			lists.set(key, list);
			return true;
		} catch (IllegalArgumentException e) {
			return false;
		}
	}
}
