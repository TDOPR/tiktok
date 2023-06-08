package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.*;

/**
 * @author Dominick Li
 * @CreateTime 2020/12/17 10:47
 * @description 对集合进行分组
 **/
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GroupByUtil {

    /**
     * List分组
     * @param <K> 返回分组Key
     * @param <V> 分组行
     */
    public interface GroupBy<K, V> {
        K groupBy(V row);
    }



    /**
     * 排序字段
     * @param <Number> 返回V的某个字段
     */
    public interface SortBy<Number, V> {
        Number sortBy(V row);
    }

    /**
     * 根据集合中的字段进行二级分类
     *
     * @param list 原始数据
     * @param one  一级分类字段
     * @param two  二级分类字段
     * @param <K>  字段类型
     * @param <V>  object对象
     * @return
     */
    public static <K, V> Map<K, Map<K, List<V>>> collectionToMapTwo(Collection<V> list, GroupBy<K, V> one, GroupBy<K, V> two) {
        Map<K, Map<K, List<V>>> resultMap = new HashMap<>();
        Map<K, List<V>> mList;
        List<V> vList;
        K o, t;
        for (V v : list) {
            o = one.groupBy(v);
            if (!resultMap.containsKey(o)) {
                mList = new HashMap<>();
                resultMap.put(o, mList);
            } else {
                mList = resultMap.get(o);
            }
            t = two.groupBy(v);
            if (!mList.containsKey(t)) {
                vList = new ArrayList<>();
                mList.put(t, vList);
            } else {
                vList = mList.get(t);
            }
            vList.add(v);
        }
        return resultMap;
    }


    /**
     * 根据集合中的某个字段进行分组
     *
     * @param list    原始数据集合
     * @param groupBy 分组的字段
     * @param <K>     字段
     * @param <V>     对象
     * @return 分组后的map集合
     */
    public static <K, V> LinkedHashMap<K, List<V>> collectionToMap(Collection<V> list, GroupBy<K, V> groupBy) {
        LinkedHashMap<K, List<V>> resultMap = new LinkedHashMap<K, List<V>>();
        for (V e : list) {
            K k = groupBy.groupBy(e);
            if (resultMap.containsKey(k)) {
                resultMap.get(k).add(e);
            } else {
                List<V> tmp = new LinkedList<V>();
                tmp.add(e);
                resultMap.put(k, tmp);
            }
        }
        return resultMap;
    }


    /**
     * 根据集合中的某个字段进行分组
     *
     * @param list    原始数据集合
     * @param groupBy 分组的字段
     * @param <K>     字段
     * @param <V>     对象
     * @return 分组后的list集合
     */
    public static <K, V> List<List<V>> collectionToList(Collection<V> list, GroupBy<K, V> groupBy) {
        Map<K, List<V>> resultMap = collectionToMap(list, groupBy);
        List<List<V>> resList = new ArrayList<>();
        for (Map.Entry<K, List<V>> entry : resultMap.entrySet()) {
            resList.add(entry.getValue());
        }
        return resList;
    }

    /**
     * list 集合分组 先分组,然后分组数据中最大的某个值排序
     *
     * @param list    待分组集合
     * @param groupBy 分组Key算法
     * @param <K>     分组Key类型
     * @param <V>     行数据类型
     * @return 分组后的Map集合
     */
    public static <K, V> Map<K, V> groupBySingle(Collection<V> list, GroupBy<K, V> groupBy, SortBy<Number, V> sortBy) {
        Map<K, List<V>> resultMap = collectionToMap(list, groupBy);
        Map<K, V> singleMap = new HashMap<>();
        resultMap.values().forEach(items -> {
            items.sort(new Comparator<V>() {
                @Override
                public int compare(V o1, V o2) {
                    if (sortBy.sortBy(o2) instanceof Integer) {
                        return sortBy.sortBy(o2).intValue() - sortBy.sortBy(o1).intValue();
                    } else {
                        return Long.compare(sortBy.sortBy(o2).longValue(), sortBy.sortBy(o1).longValue());
                    }
                }
            });
        });
        for (Map.Entry<K, List<V>> entry : resultMap.entrySet()) {
            singleMap.put(entry.getKey(), entry.getValue().get(0));
        }
        return singleMap;
    }

    /**
     * list 集合分组
     *
     * @param list    待分组集合
     * @param groupBy 分组Key算法
     * @param <K>     分组Key类型
     * @param <V>     行数据类型
     * @return 分组后的Map集合
     */
    public static <K, V> Map<K, V> groupBy(List<V> list, GroupBy<K, V> groupBy) {
        Map<K, V> resultMap = new LinkedHashMap<K, V>();
        for (V e : list) {
            K k = groupBy.groupBy(e);
            if (k != null) {
                resultMap.put(k, e);
            }
        }
        return resultMap;
    }

}
