package net.mamoe.moedb;

import cn.nukkit.utils.ConfigSection;

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
public abstract class AbstractDatabase extends LinkedHashMap<String, Object> {
    public abstract String getName();

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
    public AbstractDatabase(Map<String, ?> map) {
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
    public <T> T getObject(String key) {
        Object value = super.get(key);
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
    public <T> T getObject(String key, Class<T> valueClass) {
        Object value = super.get(key);
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
    public <T> T getObject(String key, T defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : (T) value;
    }

    public java.lang.String getString(String key, java.lang.String defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : java.lang.String.valueOf(value);
    }

    public short getShort(String key, short defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).shortValue();
    }

    public int getInteger(String key, int defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).intValue();
    }

    public long getLong(String key, long defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).longValue();
    }

    public float getFloat(String key, float defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).floatValue();
    }

    public double getDouble(String key, double defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).doubleValue();
    }

    public byte getByte(String key, byte defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : ((Number) value).byteValue();
    }

    public char getCharacter(String key, char defaultValue) {
        Object value = super.get(key);
        return value == null ? defaultValue : (char) ((Number) value).intValue();
    }

    public List<?> getList(String key) {
        return (List<?>) super.get(key);
    }

    public List<java.lang.String> getStringList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<java.lang.String> result = new ArrayList<>();
        for (java.lang.Object o : value) {
            result.add(o == null ? null : java.lang.String.valueOf(o));
        }
        return result;
    }

    public List<Integer> getIntegerList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Integer> result = new ArrayList<>();

        for (java.lang.Object object : value) {
            if (object instanceof Integer) {
                result.add((Integer) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Integer.valueOf((java.lang.String) object));
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

    public List<Boolean> getBooleanList(String key) {
        List<?> list = this.getList(key);
        if (list == null) {
            return new ArrayList<>(0);
        }
        List<Boolean> result = new ArrayList<>();
        for (java.lang.Object object : list) {
            if (object instanceof Boolean) {
                result.add((Boolean) object);
            } else if (object instanceof java.lang.String) {
                if (Boolean.TRUE.toString().equalsIgnoreCase((java.lang.String) object)) {
                    result.add(true);
                } else if (Boolean.FALSE.toString().equalsIgnoreCase((java.lang.String) object)) {
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

    public List<Double> getDoubleList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Double> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Double) {
                result.add((Double) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Double.valueOf((java.lang.String) object));
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

    public List<Float> getFloatList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Float> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Float) {
                result.add((Float) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Float.valueOf((java.lang.String) object));
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

    public List<Long> getLongList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Long> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Long) {
                result.add((Long) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Long.valueOf((java.lang.String) object));
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

    public List<Byte> getByteList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Byte> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Byte) {
                result.add((Byte) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Byte.valueOf((java.lang.String) object));
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

    public List<Character> getCharacterList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Character> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Character) {
                result.add((Character) object);
            } else if (object instanceof java.lang.String) {
                java.lang.String str = (java.lang.String) object;
                if (str.length() == 1) {
                    result.add(str.charAt(0));
                }
            } else if (object instanceof Number) {
                result.add((char) ((Number) object).intValue());
            }
        }

        return result;
    }

    public List<Short> getShortList(String key) {
        List<?> value = this.getList(key);
        if (value == null) {
            return new ArrayList<>(0);
        }
        List<Short> result = new ArrayList<>();
        for (java.lang.Object object : value) {
            if (object instanceof Short) {
                result.add((Short) object);
            } else if (object instanceof java.lang.String) {
                try {
                    result.add(Short.valueOf((java.lang.String) object));
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

    @SuppressWarnings("unchecked")
    public Map<String, Object> getMap(String key) {
        return (Map<String, Object>) super.get(key);
    }

    public Map<?, ?> getRawMap(String key) {
        return (Map<?, ?>) super.get(key);
    }


    public java.lang.String getString(String key) {
        return getString(key, null);
    }

    public short getShort(String key) {
        return getShort(key, (short) 0);
    }

    public int getInteger(String key) {
        return getInteger(key, 0);
    }

    public long getLong(String key) {
        return getLong(key, 0L);
    }

    public float getFloat(String key) {
        return getFloat(key, 0F);
    }

    public double getDouble(String key) {
        return getDouble(key, 0D);
    }

    public byte getByte(String key) {
        return getByte(key, (byte) 0);
    }

    public char getCharacter(String key) {
        return getCharacter(key, (char) 0);
    }

    /**
     * 读取子数据库
     *
     * @param key key
     *
     * @return 子数据库
     */
    public AbstractDatabase getChildDatabase(String key) {
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
                    set(java.lang.String.valueOf(entry.getKey()), entry.getValue());
                }
            }
        };
    }

    @SuppressWarnings("unchecked")
    @Override
    public AbstractDatabase clone() {
        return (AbstractDatabase) super.clone();
    }
}
