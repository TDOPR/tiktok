package com.haoliang.common.util.encrypt;

import com.haoliang.common.util.StringUtil;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MD5Util {

    /**
     * 密码拼接使用的盐
     */
    public static final String SALT = "a214eaa4d1681d10";

    public static String str2MD532(String sourceStr) throws Exception {
        String result = "";
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.update(sourceStr.getBytes());
        byte[] b = md.digest();
        int i;
        StringBuffer buf = new StringBuffer("");
        for (int offset = 0; offset < b.length; offset++) {
            i = b[offset];
            if (i < 0) {
                i += 256;
            }
            if (i < 16) {
                buf.append("0");
            }
            buf.append(Integer.toHexString(i));
        }
        result = buf.toString();
        return result;
    }

    public static String toMD5(String plainText) {
        String value = "";
        if (plainText == null) {
            plainText = "";
        }
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(plainText.getBytes());
            byte b[] = md.digest();

            int i;

            StringBuffer buf = new StringBuffer("");
            for (int offset = 0; offset < b.length; offset++) {
                i = b[offset];
                if (i < 0) {
                    i += 256;
                }
                if (i < 16) {
                    buf.append("0");
                }
                buf.append(Integer.toHexString(i));
            }
            value = buf.toString();

            // System.out.println("result: " + buf.toString());// 32位的加密
            // System.out.println("result: " + buf.toString().substring(8,
            // 24));// 16位的加密
        } catch (NoSuchAlgorithmException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();

        }
        return value;
    }

    public static String convertMD5(String inStr) {

        char[] a = inStr.toCharArray();
        for (int i = 0; i < a.length; i++) {
            a[i] = (char) (a[i] ^ 't');
        }
        String s = new String(a);
        return s;
    }

	/**
	 * 去除密码中的拼接的盐
	 */
	public static String subSlat(String pwd) {
        Integer indexOf = pwd.indexOf(SALT);
        if (indexOf != -1) {
            return StringUtil.substring(pwd, 0, indexOf);
        }
        return pwd;
    }

    public static void main(String[] agrs) {
        // md5 md51 = new md5();
        String a = MD5Util.toMD5("123456");// 加密4
        System.out.println(a);
        //System.out.println("====="+convertMD5(convertMD5("893721Wwx")));
    }
}