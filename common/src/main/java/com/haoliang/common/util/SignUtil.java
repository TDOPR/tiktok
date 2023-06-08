package com.haoliang.common.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.security.MessageDigest;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/19 15:33
 **/
@SuppressWarnings("all")
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class SignUtil {

    public static String sortData(Map<String, ?> sourceMap) {
        //log.info("sortData sourceMap:" + sourceMap);
        String returnStr = sortData(sourceMap, "&");
        //log.info("sortData returnStr:" + returnStr);
        return returnStr;
    }

    /**
     * 对数据进行排序
     * @param sourceMap 需要签名的数据
     * @param link  连接符
     * @return
     */
    public static String sortData(Map<String, ?> sourceMap, String link) {
        //log.info("start sortData method()");
        if (StringUtil.isEmpty(link)) {
            link = "&";
        }
        Map<String, Object> sortedMap = new TreeMap<String, Object>();
        sortedMap.putAll(sourceMap);
        Set<Map.Entry<String, Object>> entrySet = sortedMap.entrySet();
        StringBuffer sbf = new StringBuffer();
        for (Map.Entry<String, Object> entry : entrySet) {
            if (null != entry.getValue() && StringUtil.isNotEmpty(entry.getValue().toString())) {
                sbf.append(entry.getKey()).append("=").append(entry.getValue()).append(link);
            }
        }
        String returnStr = sbf.toString();
        if (returnStr.endsWith(link)) {
            returnStr = returnStr.substring(0, returnStr.length() - 1);
        }
        return returnStr;
    }

    /**
     *
     * @param strParams   key1=value1&key2=value2....&keyN=valueN
     * @return
     */
    public static Map parseParams(String strParams) {
        Map<String, String> map = new HashMap();
        if (!strParams.equals(""))
        {
            String[] list = strParams.split("&");
            for (int i = 0; i < list.length; i++)
            {
                String tmp = list[i];
                map.put(tmp.substring(0, tmp.indexOf("=")), tmp.substring(tmp.indexOf("=") + 1));
            }
        }
        return map;
    }

    /**
     *
     * @param signSource
     * @return
     */
    private static String calculate(String signSource) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(signSource.getBytes("utf-8"));
            byte[] b = md.digest();

            StringBuffer buf = new StringBuffer("");
            for (int offset = 0; offset < b.length; offset++) {
                int i = b[offset];
                if (i < 0)
                    i += 256;
                if (i < 16)
                    buf.append("0");
                buf.append(Integer.toHexString(i));
            }

            return buf.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static String sign(String signSource, String key) {
        if (StringUtil.isNotBlank(key)) {
            signSource = signSource + "&key=" + key;
        }
        return calculate(signSource);
    }

    /**
     * 验证签名数据是否被篡改
     * @param signSource 需要签名的数据
     * @param key 签名的key
     * @param retsign 接口返回的签名的数据
     * @return 数据是否一致
     */
    public static boolean validateSignByKey(String signSource, String key, String retsign) {
        if (StringUtil.isNotBlank(key)) {
            signSource = signSource + "&key=" + key;
        }
        String signkey = calculate(signSource);
        System.out.println("signkey======" + signkey);
        if (signkey.equals(retsign)) {
            return true;
        }
        return false;
    }
}
