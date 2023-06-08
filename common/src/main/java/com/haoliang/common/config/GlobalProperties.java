package com.haoliang.common.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @Description 全局静态变量配置
 * @Author Dominick Li
 * @CreateTime 2022/10/20 17:08
 **/
@Component
public class GlobalProperties {

    private static long tokenExpire;

    private static long adminTokenExpire;

    private static String tokenSecret;

    private static String virtualPathURL;

    private static String rootPath;

    private static boolean prodEnv;

    private static String email;

    private static String serverIp;

    private static String callBackUrl;

    public static String getServerIp() {
        return serverIp;
    }
    @Value("${app.serverIp}")
    public  void setServerIp(String serverIp) {
        GlobalProperties.serverIp = serverIp;
    }

    public static String getCallBackUrl() {
        return callBackUrl;
    }

    @Value("${app.callBackUrl}")
    public  void setCallBackUrl(String callBackUrl) {
        GlobalProperties.callBackUrl = callBackUrl;
    }

    public static String getVirtualPathURL() {
        return virtualPathURL;
    }

    @Value("${app.virtualPathURL}")
    public  void setVirtualPathURL(String virtualPathURL) {
        GlobalProperties.virtualPathURL = virtualPathURL;
    }

    public static long getTokenExpire() {
        return tokenExpire;
    }

    @Value("${jwt.expire}")
    public  void setTokenExpire(long tokenExpire) {
        GlobalProperties.tokenExpire = tokenExpire;
    }

    public static long getAdminTokenExpire() {
        return adminTokenExpire;
    }

    @Value("${jwt.adminExpire}")
    public  void setAdminTokenExpire(long adminTokenExpire) {
        GlobalProperties.adminTokenExpire = adminTokenExpire;
    }

    public static String getTokenSecret() {
        return tokenSecret;
    }

    @Value("${jwt.secret}")
    public  void setTokenSecret(String tokenSecret) {
        GlobalProperties.tokenSecret = tokenSecret;
    }

    public static String getRootPath() {
        return rootPath;
    }

    @Value("${app.rootPath}")
    public  void setRootPath(String rootPath) {
        GlobalProperties.rootPath = rootPath;
    }

    public static boolean isProdEnv() {
        return prodEnv;
    }

    @Value("${app.prodEnv:true}")
    public  void setProdEnv(boolean prodEnv) {
        GlobalProperties.prodEnv = prodEnv;
    }

    public static String getEmail() {
        return email;
    }

    @Value("${mail.username}")
    public  void setEmail(String email) {
        GlobalProperties.email = email;
    }
}
