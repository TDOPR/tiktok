package com.haoliang.pay.vn;

import org.apache.commons.lang3.StringUtils;

import java.nio.charset.Charset;
import java.util.*;

/**
 * @author zeming.fan@swiftpass.cn
 */
public class SftSignUtil {

    public final static Charset UTF_8 = Charset.forName("UTF-8");

    public static String formatSignData(Map<String, String> signDataMap) {
        Set<String> sortedSet = new TreeSet<String>(signDataMap.keySet());
        StringBuffer sb = new StringBuffer();
        for (String key : sortedSet) {
            if ("sign".equalsIgnoreCase(key)) {
                continue;
            }
            if (signDataMap.get(key) != null) {
                String v = String.valueOf(signDataMap.get(key));
                if (StringUtils.isNotBlank(v)) {
                    sb.append(key);
                    sb.append("=");
                    sb.append(v);
                    sb.append("&");
                }
            }
        }
        String s = sb.toString();
        if (s.length() > 0) {
            s = s.substring(0, s.length() - 1);
        }
//		log.debug("To be signed data: {}", s);
        return s;
    }

    public static boolean verifySign(String signData, String sign, String authMchPublicKey) {
        return md5VerifySign(signData, sign, authMchPublicKey);
    }


    public static boolean md5VerifySign(String signData, String sign, String authMchPublicKey) {
        String lsign = md5Sign(signData, authMchPublicKey);
        return lsign.equals(sign.toLowerCase());
    }

    public static String sign(String signData, String sftPrivateKey) throws Exception {
        return md5Sign(signData, sftPrivateKey);
    }


    public static String md5Sign(String signData, String sftPrivateKey) {
        String sign = PayMD5.MD5Encode(signData + sftPrivateKey).toLowerCase();
        return sign;
    }


    public static void main(String[] args) throws Exception {
        Map<String, String> param = new HashMap<>();
        param.put("service", "PayCreateOrder");
        param.put("merchant_id", "API23777051326929450");
        param.put("nonce_str", "12345");
        param.put("notify_url", "http://47.242.107.138:9091/callBack");
        param.put("return_url", "");
        param.put("order_no", "12345");
        param.put("biz_amt", "100");
        //需要获取API對接菜單獲取，通道類型對應的ID編號
        param.put("biz_type", "101");
        param.put("bankBranchName", "V_Vietcombank");
        String sortData = SftSignUtil.formatSignData(param);
        System.out.println(sortData);
        String sing = SftSignUtil.sign( sortData, "0254563235284b28bd522ba4da51497e");
        System.out.println(sing);
    }

}
