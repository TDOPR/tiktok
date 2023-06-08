package com.haoliang.test;

import cn.hutool.core.map.BiMap;

import java.util.HashMap;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/15 16:45
 **/
public class TestBiMap {
    public static void main(String[] args) {
        String token0 = "美元";
        String token1 = "人民币";
        String token2 = "欧元";
        String token3 = "英镑";
        String token4 = "日元";
        HashMap<String, Integer> map = new HashMap();
        map.put(token0, 0);
        map.put(token1, 1);
        map.put(token2, 2);
        map.put(token3, 3);
        map.put(token4, 4);
        BiMap<String,Integer> biMap=new BiMap<>(map);
        System.out.println(biMap.getKey(1));
        System.out.println(biMap.get("日元"));
    }
}
