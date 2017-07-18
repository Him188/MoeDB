package net.mamoe.moedb;

import java.util.Map;

/**
 * @author Him188 @ MoeDB Project
 */
public interface Database<K, V> extends Map<K, V> {
	String getName();
}