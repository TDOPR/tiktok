//package com.haoliang.common.util.redis;
//
//import com.haoliang.common.util.SpringUtil;
//import lombok.AccessLevel;
//import lombok.NoArgsConstructor;
//import org.redisson.api.RMap;
//import org.springframework.cache.Cache;
//import org.springframework.cache.CacheManager;
//
//import java.util.Set;
//
///**
// * 缓存操作工具类
// */
//@NoArgsConstructor(access = AccessLevel.PRIVATE)
//@SuppressWarnings(value = {"unchecked"})
//public class CacheUtil {
//
//
//    /**
//     * 获取缓存组内所有的KEY
//     *
//     * @param cacheNames 缓存组名称
//     */
//    public static Set<Object> keys(String cacheNames) {
//        RMap<Object, Object> rmap = (RMap<Object, Object>) getCacheManager().getCache(cacheNames).getNativeCache();
//        return rmap.keySet();
//    }
//
//    /**
//     * 获取缓存值
//     *
//     * @param cacheNames 缓存组名称
//     * @param key        缓存key
//     */
//    public static <T> T get(String cacheNames, Object key) {
//        Cache.ValueWrapper wrapper = getCacheManager().getCache(cacheNames).get(key);
//        return wrapper != null ? (T) wrapper.get() : null;
//    }
//
//    /**
//     * 保存缓存值
//     *
//     * @param cacheNames 缓存组名称
//     * @param key        缓存key
//     * @param value      缓存值
//     */
//    public static void put(String cacheNames, Object key, Object value) {
//        getCacheManager().getCache(cacheNames).put(key, value);
//    }
//
//    /**
//     * 删除缓存值
//     *
//     * @param cacheNames 缓存组名称
//     * @param key        缓存key
//     */
//    public static void evict(String cacheNames, Object key) {
//        getCacheManager().getCache(cacheNames).evict(key);
//    }
//
//    /**
//     * 清空缓存值
//     *
//     * @param cacheNames 缓存组名称
//     */
//    public static void clear(String cacheNames) {
//        getCacheManager().getCache(cacheNames).clear();
//    }
//
//    public static CacheManager getCacheManager() {
//        return CacheUtil.Lazy.CACHE_MANAGER;
//    }
//
//    /**
//     * 使用懒加载方式实例化CacheManager
//     */
//    private static class Lazy {
//        private static final CacheManager CACHE_MANAGER = SpringUtil.getBean(CacheManager.class);
//    }
//
//}
