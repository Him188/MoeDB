package net.mamoe.moedb;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Him188 @ MoeDB Project
 */
public class Utils {
	@SuppressWarnings("unchecked")
	public static <T> T defaults(T value, T defaultValue) {
		return value == null ||
				(value instanceof Map && !((Map) value).isEmpty()) ||
				(value instanceof List && !((List) value).isEmpty()) ? defaultValue : value;
	}

	@SuppressWarnings("unchecked")
	public static long defaults(long value, long defaultValue) {
		return value == 0 ? defaultValue : value;
	}

	public static <T> int listFind(List<T> list, T value) {
		for (int i = 0; i < list.size(); i++) {
			if (list.get(i) == value) {
				return i;
			}
		}

		return -1;
	}

	public static <T> LinkedList<T> listReverse(List<T> list) {
		LinkedList<T> result = new LinkedList<>();
		for (int i = list.size(); i > 0; i--) {
			result.add(list.get(i - 1));
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public static <T> T[] arrayReverse(T[] list) {
		T[] result = (T[]) java.lang.reflect.Array.newInstance(list.getClass().getComponentType(), list.length);
		int ii = 0;
		for (int i = list.length; i > 0; i--) {
			result[ii++] = list[i - 1];
		}
		return result;
	}
}
