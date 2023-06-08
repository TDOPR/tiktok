package com.haoliang.common.util.google;

import org.apache.commons.codec.binary.Base32;
import org.apache.commons.codec.binary.Base64;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

import static com.haoliang.common.util.google.QRCodeUtil.generateQRCodeImg;


/**
 * @author Dominick Li
 * @Description 谷歌认证器秘钥生成工具
 * @CreateTime 2022/11/21 14:28
 **/
public class GoogleAuthenticatorUtil {
    public static final int SECRET_SIZE = 10;

    public static final String SEED = "g8GjEvTbW5oVSV7avLBdwIHqGlUYNzKFI7izOF8GwLDVKs2m0QN7vxRs2im5MDaNCWGmcD2rvcZx";

    public static final String RANDOM_NUMBER_ALGORITHM = "SHA1PRNG";

    /**
     * default 3 - max 17 (from google docs)最多可偏移的时间
     */
    int window_size = 3;

    public void setWindowSize(int s) {
        if (s >= 1 && s <= 17) {
            window_size = s;
        }
    }

    /**
     * 验证身份验证码是否正确
     *
     * @param codes       输入的身份验证码
     * @param savedSecret 密钥
     * @return
     */
    public static Boolean authcode(String codes, String savedSecret) {
        long code = 0;
        try {
            code = Long.parseLong(codes);
        } catch (Exception e) {
            e.printStackTrace();
        }
        long t = System.currentTimeMillis();
        GoogleAuthenticatorUtil ga = new GoogleAuthenticatorUtil();

        // should give 5 * 30 seconds of grace...
        ga.setWindowSize(ga.window_size);

        return ga.check_code(savedSecret, code, t);
    }

    /**
     * 获取密钥
     *
     * @param user 用户
     * @param host 域
     * @return 密钥
     */
    public static String genSecret(String user, String host) {
        String secret = GoogleAuthenticatorUtil.generateSecretKey();
        //GoogleAuthenticatorUtil.getQRBarcodeURL(user, host, secret);
        return secret;
    }

    /**
     * 生成密钥
     *
     * @return
     */
    private static String generateSecretKey() {
        SecureRandom sr = null;
        try {
            sr = SecureRandom.getInstance(RANDOM_NUMBER_ALGORITHM);
            sr.setSeed(Base64.decodeBase64(SEED));
            byte[] buffer = sr.generateSeed(SECRET_SIZE);
            Base32 codec = new Base32();
            byte[] bEncodedKey = codec.encode(buffer);
            String encodedKey = new String(bEncodedKey);
            return encodedKey;
        } catch (NoSuchAlgorithmException e) {
            // should never occur... configuration error
        }
        return null;
    }

    /**
     * 获取二维码内容URL
     *
     * @param user   用户
     * @param host   域
     * @param secret 密钥
     * @return 二维码URL
     */
    public static String getQRBarcodeURL(String user, String host, String secret) {
        String format = "otpauth://totp/%s@%s?secret=%s";
        return String.format(format, user, host, secret);
    }

    /**
     * 校验code是否正确
     *
     * @param secret   密钥
     * @param code     动态code
     * @param timeMsec 时间
     * @return
     */
    private boolean check_code(String secret, long code, long timeMsec) {
        Base32 codec = new Base32();
        byte[] decodedKey = codec.decode(secret);
        long t = (timeMsec / 1000L) / 30L;
        for (int i = -window_size; i <= window_size; ++i) {
            long hash;
            try {
                hash = verify_code(decodedKey, t + i);
            } catch (Exception e) {
                e.printStackTrace();
                throw new RuntimeException(e.getMessage());
            }
            if (hash == code) {
                return true;
            }
        }
        return false;
    }

    /**
     * 时间校验密钥与code是否匹配
     *
     * @param key 解密后的密钥
     * @param t   时间
     * @return
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    private static int verify_code(byte[] key, long t)
            throws NoSuchAlgorithmException, InvalidKeyException {
        byte[] data = new byte[8];
        long value = t;
        for (int i = 8; i-- > 0; value >>>= 8) {
            data[i] = (byte) value;
        }
        SecretKeySpec signKey = new SecretKeySpec(key, "HmacSHA1");
        Mac mac = Mac.getInstance("HmacSHA1");
        mac.init(signKey);
        byte[] hash = mac.doFinal(data);
        int offset = hash[20 - 1] & 0xF;
        long truncatedHash = 0;
        for (int i = 0; i < 4; ++i) {
            truncatedHash <<= 8;
            truncatedHash |= (hash[offset + i] & 0xFF);
        }
        truncatedHash &= 0x7FFFFFFF;
        truncatedHash %= 1000000;
        return (int) truncatedHash;
    }


    public static void main(String[] args) {
        /*
         * 注意：先运行前两步，获取密钥和二维码url。 然后只运行第三步，填写需要验证的验证码，和第一步生成的密钥
         */
        String user = "admin-prod";
        String host = "tiktokguild.google.com";
        // 第一步：获取密钥
        String secret = genSecret(user, host);
        System.out.println("secret:" + secret);
        // 第二步：根据密钥获取二维码图片url（可忽略） GY2B53GEGH4YWNOV
        String url = getQRBarcodeURL(user, host, secret);
        System.out.println("url:" + url);
        // 第三步 生成二维码  UHEBOCCZ4L2DSRYB
        generateQRCodeImg("C:\\Users\\Administrator\\Desktop\\", url);
        //第四步 验证验证码有没有错误
        String code = "660563"; //谷歌认证器app中显示的验证码
        boolean result = authcode(code, secret);
        System.out.println("result:" + result);
    }
}
