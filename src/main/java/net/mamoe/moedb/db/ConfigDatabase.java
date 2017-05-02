package net.mamoe.moedb.db;

import java.util.List;
import java.util.Map;

/**
 * @author Him188 @ MoeDB Project
 * @since MoeDB
 */
public class ConfigDatabase implements KeyValueDatabase {
	public String getName() {
		return null;
	}

	@Override
	public Object get(String key) {
		return null;
	}

	@Override
	public Object get(String key, Object defaultValue) {
		return null;
	}

	@Override
	public boolean set(String key, Object value) {
		return false;
	}

	@Override
	public Object getSet(String key, Object value) {
		return null;
	}

	@Override
	public Object getSet(String key, Object value, Object defaultValue) {
		return null;
	}

	@Override
	public List<String> multiGet(String... keys) {
		return null;
	}

	@Override
	public List<String> multiGet(List<String> defaultValue, String... keys) {
		return null;
	}

	@Override
	public boolean multiSet(String... keys_values) {
		return false;
	}

	@Override
	public boolean hashDelete(String key, String fields) {
		return false;
	}

	@Override
	public boolean hashExits(String key, String field) {
		return false;
	}

	@Override
	public String hashGet(String key, String field) {
		return null;
	}

	@Override
	public String hashGet(String key, String field, String defaultValue) {
		return null;
	}

	@Override
	public Map<String, String> hashGetAll(String key) {
		return null;
	}

	@Override
	public Map<String, String> hashGetAll(String key, Map<String, String> defaultValue) {
		return null;
	}

	@Override
	public long hashGetLength(String key) {
		return 0;
	}

	@Override
	public long hashGetLength(String key, long defaultValue) {
		return 0;
	}

	@Override
	public List<String> hashMultiGet(String key, String... fields) {
		return null;
	}

	@Override
	public List<String> hashMultiGet(String key, List<String> defaultValue, String... fields) {
		return null;
	}

	@Override
	public boolean hashSet(String key, Map<String, String> value) {
		return false;
	}

	@Override
	public boolean hashSet(String key, String field, String value) {
		return false;
	}

	@Override
	public boolean hashSetIfNx(String key, String field, String value) {
		return false;
	}

	@Override
	public List<String> hashGetValues(String key) {
		return null;
	}

	@Override
	public List<String> hashGetValues(String key, List<String> defaultValue) {
		return null;
	}

	@Override
	public List<String> listLeftPop(int timeout, String... keys) {
		return null;
	}

	@Override
	public List<String> listRightPop(int timeout, String... keys) {
		return null;
	}

	@Override
	public String listGet(String key, long index) {
		return null;
	}

	@Override
	public String listGet(String key, long index, String defaultValue) {
		return null;
	}

	@Override
	public boolean listInsert(String key, boolean position, String existing_value, String value) {
		return false;
	}

	@Override
	public long listLength(String key) {
		return 0;
	}

	@Override
	public long listLength(String key, long defaultValue) {
		return 0;
	}

	@Override
	public String listLeftPop(String key) {
		return null;
	}

	@Override
	public String listLeftPop(String key, String defaultValue) {
		return null;
	}

	@Override
	public String listRightPop(String key) {
		return null;
	}

	@Override
	public String listRightPop(String key, String defaultValue) {
		return null;
	}

	@Override
	public boolean listLeftPush(String key, String... value) {
		return false;
	}

	@Override
	public boolean listLeftPushIfNx(String key, String... value) {
		return false;
	}

	@Override
	public boolean listRightPush(String key, String... value) {
		return false;
	}

	@Override
	public boolean listRightPushIfNx(String key, String... value) {
		return false;
	}

	@Override
	public List<String> listRange(String key, long start, long end) {
		return null;
	}

	@Override
	public List<String> listRange(String key, long start) {
		return null;
	}

	@Override
	public List<String> listGetAll(String key) {
		return null;
	}

	@Override
	public long listRemove(String key, long count, String value) {
		return 0;
	}

	@Override
	public long listDelete(String key, long count, String value) {
		return 0;
	}

	@Override
	public long listRemove(String key, String value) {
		return 0;
	}

	@Override
	public long listDelete(String key, String value) {
		return 0;
	}

	@Override
	public boolean listSet(String key, long index, String value) {
		return false;
	}

	@Override
	public boolean listTrim(String key, long start, long end) {
		return false;
	}
}
