package net.mamoe.moedb;

import cn.nukkit.utils.ConfigSection;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 数据库基类. <br>
 *
 * @author Him188 @ MoeDB Project
 */
@SuppressWarnings({"unused", "WeakerAccess", "SameParameterValue"})
public abstract class AbstractDatabase<K, V> extends LinkedHashMap<K, V> implements Database<K, V> {
    /**
     * 构造 {@link AbstractDatabase} 实例, 数据库中初始数据为空
     */
    public AbstractDatabase() {
        super();
    }

    /**
     * 构造 {@link AbstractDatabase} 实例, 并以 <code>map</code> 的数据作为数据库初始数据
     *
     * @param map 初始数据
     */
    public AbstractDatabase(Map<? extends K, ? extends V> map) {
        super(map);
    }

    /**
     * 读取 <code>key</code> 对应的值并强转为需求的类型
     *
     * @param key 键
     * @param <T> 值的类型
     *
     * @return <code>key</code> 对应的值, <code>key</code> 不存在时返回 null
     */
    @SuppressWarnings("unchecked")
    public <T> T getObject(K key) {
        V value = super.get(key);
        return value == null ? null : (T) value;
    }

    /**
     * 读取 <code>key</code> 对应的值并强转为 <code>valueClass</code> 的类型
     *
     * @param key        键
     * @param valueClass 值的类型 Class
     * @param <T>        值的类型
     *
     * @return <code>key</code> 对应的值, <code>key</code> 不存在时返回 null
     */
    @SuppressWarnings("unchecked")
    public <T> T getObject(K key, Class<T> valueClass) {
        V value = super.get(key);
        return value == null ? null : (T) value;
    }

    /**
     * 读取 <code>key</code> 对应的值并强转为 <code>defaultValue</code> 的类型
     *
     * @param key          键
     * @param defaultValue <code>key</code> 对应的值为 <code>null</code> 时的默认值
     * @param <T>          值的类型
     *
     * @return <code>key</code> 对应的值不为 <code>null</code> 时返回这个值, 否则返回 <code>defaultValue</code>
     */
    @SuppressWarnings("unchecked")
    public <T> T getObject(K key, T defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : (T) value;
    }

    public String getString(K key, String defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : String.valueOf(value);
    }

    public short getShort(K key, short defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).shortValue();
    }

    public int getInteger(K key, int defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).intValue();
    }

    public long getLong(K key, long defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).longValue();
    }

    public float getFloat(K key, float defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).floatValue();
    }

    public double getDouble(K key, double defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).doubleValue();
    }

    public byte getByte(K key, byte defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : ((Number) value).byteValue();
    }

    public char getCharacter(K key, char defaultValue) {
        V value = super.get(key);
        return value == null ? defaultValue : (char) ((Number) value).intValue();
    }


    public List<?> getRawList(K key) {
        return (List<?>) super.get(key);
    }

    public List<String> getStringList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<String> result = new ArrayList<>();
        for (Object o : value) {
            result.add(o == null ? null : String.valueOf(o));
        }
        return result;
    }

    public List<Integer> getIntegerList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Integer> result = new ArrayList<>();

        for (Object object : value) {
            if (object instanceof Integer) {
                result.add((Integer) object);
            } else if (object instanceof String) {
                try {
                    result.add(Integer.valueOf((String) object));
                } catch (Exception ignored) {
                }
            } else if (object instanceof Character) {
                result.add((int) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).intValue());
            }
        }
        return result;
    }

    public List<Boolean> getBooleanList(K key) {
        List<?> list = this.getRawList(key);
        if (list == null) {
            return new ArrayList<>(0);
        }
        List<Boolean> result = new ArrayList<>();
        for (Object object : list) {
            if (object instanceof Boolean) {
                result.add((Boolean) object);
            } else if (object instanceof String) {
                if (Boolean.TRUE.toString().equalsIgnoreCase((String) object)) {
                    result.add(true);
                } else if (Boolean.FALSE.toString().equalsIgnoreCase((String) object)) {
                    result.add(false);
                }
            } else if (object instanceof Number) {
                int intValue = ((Number) object).intValue();
                if (intValue == 1) {
                    result.add(true);
                } else if (intValue == 0) {
                    result.add(false);
                }
            }
        }
        return result;
    }

    public List<Double> getDoubleList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Double> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Double) {
                result.add((Double) object);
            } else if (object instanceof String) {
                try {
                    result.add(Double.valueOf((String) object));
                } catch (Exception ignored) {
                }
            } else if (object instanceof Character) {
                result.add((double) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).doubleValue());
            }
        }
        return result;
    }

    public List<Float> getFloatList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Float> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Float) {
                result.add((Float) object);
            } else if (object instanceof String) {
                try {
                    result.add(Float.valueOf((String) object));
                } catch (Exception ex) {
                    //ignore
                }
            } else if (object instanceof Character) {
                result.add((float) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).floatValue());
            }
        }
        return result;
    }

    public List<Long> getLongList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Long> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Long) {
                result.add((Long) object);
            } else if (object instanceof String) {
                try {
                    result.add(Long.valueOf((String) object));
                } catch (Exception ex) {
                    //ignore
                }
            } else if (object instanceof Character) {
                result.add((long) (Character) object);
            } else if (object instanceof Number) {
                result.add(((Number) object).longValue());
            }
        }
        return result;
    }

    public List<Byte> getByteList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Byte> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Byte) {
                result.add((Byte) object);
            } else if (object instanceof String) {
                try {
                    result.add(Byte.valueOf((String) object));
                } catch (Exception ignored) {
                }
            } else if (object instanceof Character) {
                result.add((byte) ((Character) object).charValue());
            } else if (object instanceof Number) {
                result.add(((Number) object).byteValue());
            }
        }

        return result;
    }

    public List<Character> getCharacterList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Character> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Character) {
                result.add((Character) object);
            } else if (object instanceof String) {
                String str = (String) object;
                if (str.length() == 1) {
                    result.add(str.charAt(0));
                }
            } else if (object instanceof Number) {
                result.add((char) ((Number) object).intValue());
            }
        }

        return result;
    }

    public List<Short> getShortList(K key) {
        List<?> value = this.getRawList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Short> result = new ArrayList<>();
        for (Object object : value) {
            if (object instanceof Short) {
                result.add((Short) object);
            } else if (object instanceof String) {
                try {
                    result.add(Short.valueOf((String) object));
                } catch (Exception ignored) {
                }
            } else if (object instanceof Character) {
                result.add((short) ((Character) object).charValue());
            } else if (object instanceof Number) {
                result.add(((Number) object).shortValue());
            }
        }

        return result;
    }

    public Map<?, ?> getRawMap(K key) {
        return (Map<?, ?>) super.get(key);
    }


    public String getString(K key) {
        return getString(key, null);
    }

    public short getShort(K key) {
        return getShort(key, (short) 0);
    }

    public int getInteger(K key) {
        return getInteger(key, 0);
    }

    public long getLong(K key) {
        return getLong(key, 0L);
    }

    public float getFloat(K key) {
        return getFloat(key, 0F);
    }

    public double getDouble(K key) {
        return getDouble(key, 0D);
    }

    public byte getByte(K key) {
        return getByte(key, (byte) 0);
    }

    public char getCharacter(K key) {
        return getCharacter(key, (char) 0);
    }

    /**
     * 读取子数据库
     *
     * @param key key
     *
     * @return 子数据库
     */
    public AbstractDatabase<K, V> getChildDatabase(K key) {
        throw new UnsupportedOperationException();
    }

    /**
     * 返回一个拥有数据库所有数据的 {@link ConfigSection}
     *
     * @return Nukkit config section({@link ConfigSection})
     */
    @SuppressWarnings("unchecked")
    public ConfigSection toConfigSection() {

        return new ConfigSection() {
            {
                for (Map.Entry<?, ?> entry : AbstractDatabase.this.entrySet()) {
                    set(String.valueOf(entry.getKey()), entry.getValue());
                }
            }
        };
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractDatabase<K, V> clone() {
        return (AbstractDatabase<K, V>) super.clone();
    }
}
