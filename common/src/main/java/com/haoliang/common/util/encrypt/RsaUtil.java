package com.haoliang.common.util.encrypt;

import com.haoliang.common.constant.RSAKeyConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;

import javax.crypto.Cipher;
import java.io.ByteArrayOutputStream;
import java.security.*;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/21 15:13
 **/
@Slf4j
public class RsaUtil {
    //加密算法RSA
    private static final String KEY_ALGORITHM = "RSA";
    //RSA最大加密明文大小
    private static final int MAX_ENCRYPT_BLOCK = 117;
    //RSA最大解密密文大小
    private static final int MAX_DECRYPT_BLOCK = 128;
    private static final String CHARSET = "UTF-8";

    public static Map<String, String> genKeyPair() throws NoSuchAlgorithmException {
        // KeyPairGenerator类用于生成公钥和私钥对，基于RSA算法生成对象
        KeyPairGenerator keyPairGen = KeyPairGenerator.getInstance("RSA");
        // 初始化密钥对生成器，密钥大小为96-1024位
        keyPairGen.initialize(1024);
        // 生成一个密钥对，保存在keyPair中
        KeyPair keyPair = keyPairGen.generateKeyPair();
        RSAPrivateKey privateKey = (RSAPrivateKey) keyPair.getPrivate();   // 得到私钥
        RSAPublicKey publicKey = (RSAPublicKey) keyPair.getPublic();  // 得到公钥
        String publicKeyString = new String(Base64.encodeBase64(publicKey.getEncoded()));
        // 得到私钥字符串
        String privateKeyString = new String(Base64.encodeBase64((privateKey.getEncoded())));
        // 将公钥和私钥保存到Map
        Map<String, String> retMap = new HashMap<>();
        retMap.put("pubKey", publicKeyString);
        retMap.put("priKey", privateKeyString);
        return retMap;
    }

    /**
     * 得到私钥
     *
     * @param privateKey 密钥字符串（经过base64编码）
     * @throws Exception
     */
    public static RSAPrivateKey getPrivateKey(String privateKey) throws NoSuchAlgorithmException, InvalidKeySpecException {
        //通过PKCS#8编码的Key指令获得私钥对象
        KeyFactory keyFactory = KeyFactory.getInstance(KEY_ALGORITHM);
        PKCS8EncodedKeySpec pkcs8KeySpec = new PKCS8EncodedKeySpec(Base64.decodeBase64(privateKey));
        RSAPrivateKey key = (RSAPrivateKey) keyFactory.generatePrivate(pkcs8KeySpec);
        return key;
    }

    /**
     * 得到公钥
     *
     * @param publicKey 密钥字符串（经过base64编码）
     * @throws Exception
     */
    public static RSAPublicKey getPublicKey(String publicKey) throws NoSuchAlgorithmException, InvalidKeySpecException {
        //通过X509编码的Key指令获得公钥对象
        KeyFactory keyFactory = KeyFactory.getInstance(KEY_ALGORITHM);
        X509EncodedKeySpec x509KeySpec = new X509EncodedKeySpec(Base64.decodeBase64(publicKey));
        RSAPublicKey key = (RSAPublicKey) keyFactory.generatePublic(x509KeySpec);
        return key;
    }

    /**
     * @param signSrc    原始数据
     * @param cipherText 待解密数据
     * @param publickey  公钥
     * @return
     * @throws Exception
     */
    public static boolean verifySignByPub(String signSrc, String cipherText, String publickey) {
        Boolean verifySign = false;
        try {
            String decryptSign = decryptByPublic(cipherText, publickey);
            if (signSrc.equalsIgnoreCase(decryptSign)) {
                verifySign = true;
            }
        } catch (Exception e) {
            log.error("公钥解密时出现异常:{}", e.getMessage());
        }
        return verifySign;
    }

    /**
     * 私钥加密
     *
     * @param data       源
     * @param privateKey 私钥
     * @return
     */
    public static String encryptByPrivate(String data, String privateKey) {
        try {
            //获取私钥
            RSAPrivateKey rsaPrivateKey = getPrivateKey(privateKey);
            Cipher cipher = Cipher.getInstance(KEY_ALGORITHM);
            cipher.init(Cipher.ENCRYPT_MODE, rsaPrivateKey);
            return Base64.encodeBase64URLSafeString(rsaSplitCodec(cipher, Cipher.ENCRYPT_MODE, data.getBytes(CHARSET), rsaPrivateKey.getModulus().bitLength()));
        } catch (Exception e) {
            throw new RuntimeException("私钥加密字符串[" + data + "]时异常", e);
        }
    }



    /**
     * 公钥解密
     *
     * @param data      源
     * @param publicKey 公钥
     * @return
     */
    public static String decryptByPublic(String data, String publicKey) {
        try {
            RSAPublicKey rsaPublicKey = getPublicKey(publicKey);
            Cipher cipher = Cipher.getInstance(KEY_ALGORITHM);
            cipher.init(Cipher.DECRYPT_MODE, rsaPublicKey);
            return new String(rsaSplitCodec(cipher, Cipher.DECRYPT_MODE, Base64.decodeBase64(data), rsaPublicKey.getModulus().bitLength()), "UTF-8");
        } catch (Exception e) {
            throw new RuntimeException("公钥解密字符串[" + data + "]时异常", e);
        }
    }

    /**
     * 数据分段加密
     *
     * @param cipher
     * @param opmode
     * @param datas
     * @param keySize
     * @return
     * @throws Exception
     */
    private static byte[] rsaSplitCodec(Cipher cipher, int opmode, byte[] datas, int keySize) throws Exception {
        int maxBlock = 0;
        if (opmode == Cipher.DECRYPT_MODE) {
            maxBlock = keySize / 8;
        } else {
            maxBlock = keySize / 8 - 11;
        }

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        int offSet = 0;
        byte[] buff;
        int i = 0;
        byte[] resultDatas = null;
        try {
            while (datas.length > offSet) {
                if (datas.length - offSet > maxBlock) {
                    buff = cipher.doFinal(datas, offSet, maxBlock);
                } else {
                    buff = cipher.doFinal(datas, offSet, datas.length - offSet);
                }
                out.write(buff, 0, buff.length);
                i++;
                offSet = i * maxBlock;
            }
            resultDatas = out.toByteArray();
        } catch (Exception e) {
            throw new RuntimeException("加解密阀值为[" + maxBlock + "]的数据时发生异常", e);
        } finally {
            out.close();
        }
        return resultDatas;
    }


    /**
     * 数据排序
     *
     * @param keys
     * @return
     */
    public static String[] getUrlParam(String[] keys) {

        for (int i = 0; i < keys.length - 1; i++) {
            for (int j = 0; j < keys.length - i - 1; j++) {
                String pre = keys[j];
                String next = keys[j + 1];
                if (isMoreThan(pre, next)) {
                    String temp = pre;
                    keys[j] = next;
                    keys[j + 1] = temp;
                }
            }
        }
        return keys;
    }


    private static boolean isMoreThan(String pre, String next) {
        if (null == pre || null == next || "".equals(pre) || "".equals(next)) {
            return false;
        }

        char[] c_pre = pre.toCharArray();
        char[] c_next = next.toCharArray();

        int minSize = Math.min(c_pre.length, c_next.length);

        for (int i = 0; i < minSize; i++) {
            if ((int) c_pre[i] > (int) c_next[i]) {
                return true;
            } else if ((int) c_pre[i] < (int) c_next[i]) {
                return false;
            }
        }
        if (c_pre.length > c_next.length) {
            return true;
        }

        return false;
    }

    public static void main(String[] args) throws Exception {
        String source = "123456";
        //公钥加密
        String encript = encryptByPrivate(RSAKeyConstants.PRIVATE_KEY, source);
        System.out.println("加密后数据：" + encript+",数据长度="+encript.length());

        String oldSource = decryptByPublic(RSAKeyConstants.PUBLIC_KEY, encript);
        System.out.println("解密后数据:" + oldSource);
    }




}
