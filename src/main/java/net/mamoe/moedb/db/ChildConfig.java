package net.mamoe.moedb.db;

import cn.nukkit.utils.Config;
import cn.nukkit.utils.ConfigSection;

import java.util.Map;

/**
 * @author Him188 @ MoeDB Project
 */
public class ChildConfig extends ConfigSection {
	private final String sectionName;
	private final Config base;
	private final boolean autoSave;

	ChildConfig(String sectionName, Config base, boolean autoSave) {
		this.sectionName = sectionName;
		this.base = base;
		this.autoSave = autoSave;

		this.putAll(base.getSection(sectionName));
	}

	public Config getBaseConfig() {
		return base;
	}

	@Override
	public Object put(String key, Object value) {
		Object v = super.put(key, value);

		base.set(sectionName, this);

		if (autoSave) base.save();
		return v;
	}

	@Override
	public Object putIfAbsent(String key, Object value) {
		Object v = super.putIfAbsent(key, value);

		base.set(sectionName, this);
		if (autoSave) base.save();
		return v;
	}

	@Override
	public void putAll(Map<? extends String, ?> m) {
		super.putAll(m);

		base.set(sectionName, this);
		if (autoSave) base.save();
	}
}
