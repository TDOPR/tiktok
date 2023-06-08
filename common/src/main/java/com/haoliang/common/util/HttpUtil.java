package com.haoliang.common.util;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.model.HttpResult;
import okhttp3.*;

import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author Dominick Li
 * @createTime 2020/3/19 18:33
 * @description http请求工具类
 **/
public class HttpUtil {

    /**
     * 需要导入okhttp依赖
     * <dependency>
     * <groupId>com.squareup.okio</groupId>
     * <artifactId>okio</artifactId>
     * <version>1.13.0</version>
     * </dependency>
     * <dependency>
     * <groupId>com.squareup.okhttp3</groupId>
     * <artifactId>okhttp</artifactId>
     * <version>3.12.0</version>
     * </dependency>
     */
    private HttpUtil() {
    }

    /**
     * post请求
     * json数据格式
     *
     * @param url      接口路径
     * @param jsonData json数据
     * @param tClass   json数据对应的泛型类
     */
    public static <T> HttpResult<T> postJson(String url, String jsonData, Class<T> tClass) {
        return postJson(url, Collections.EMPTY_MAP, jsonData, tClass);
    }

    /**
     * post请求
     * json数据格式
     *
     * @param url      接口路径
     * @param headers  请求头
     * @param jsonData json数据
     * @param tClass   json数据对应的泛型类
     */
    public static <T> HttpResult<T> postJson(String url, Map<String, String> headers, String jsonData, Class<T> tClass) {
        try {
            Request.Builder builder = new Request.Builder().url(url);
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                builder.addHeader(entry.getKey(), entry.getValue());
            }
            RequestBody requestBody = FormBody.create(MediaType.parse("application/json; charset=utf-8"), jsonData);
            builder.post(requestBody);
            Request request = builder.build();
            Response response = getInstance().newCall(request).execute();
            String result = response.body().string();
            System.out.println(result);
            if (response.isSuccessful()) {
                return HttpResult.successResult(JSONObject.parseObject(result, tClass));
            }
            return HttpResult.failureResult(result);
        } catch (Exception e) {
            e.printStackTrace();
            return HttpResult.failureResult(e.getMessage());
        }
    }

    /**
     * post请求
     *
     * @param url      接口路径
     * @param jsonData json数据
     */
    public static HttpResult<String> postJson(String url, String jsonData) {
        return postJson(url, Collections.EMPTY_MAP, jsonData);
    }

    public static HttpResult<String> postJson(String url, Map<String, String> headers, String jsonData) {
        try {
            Request.Builder builder = new Request.Builder().url(url);
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                builder.addHeader(entry.getKey(), entry.getValue());
            }
            RequestBody requestBody = FormBody.create(MediaType.parse("application/json; charset=utf-8"), jsonData);
            builder.post(requestBody);
            Request request = builder.build();
            Response response = getInstance().newCall(request).execute();
            String result = response.body().string();
            System.out.println(result);
            if (response.isSuccessful()) {
                return HttpResult.successResult(result);
            }
            return HttpResult.failureResult(result);
        } catch (Exception e) {
            e.printStackTrace();
            return HttpResult.failureResult(e.getMessage());
        }
    }


    /**
     * post请求
     * 数据格式  表单
     *
     * @param url   接口路径
     * @param param 请求参数
     */
    public static String post(String url, Map<String, String> param) {
        return post(url, Collections.EMPTY_MAP, param);
    }


    /**
     * post请求
     * 数据格式  表单
     *
     * @param url     接口路径
     * @param headers 请求头
     * @param param   请求参数
     */
    public static String post(String url, Map<String, String> headers, Map<String, String> param) {
        try {
            URL realUrl = new URL(url);
            FormBody.Builder builder = new FormBody.Builder();
            FormBody mBody = builder.build();
            Request.Builder requestBuilder = new Request.Builder().url(realUrl).post(mBody);
            for (Map.Entry<String, String> header : headers.entrySet()) {
                requestBuilder.addHeader(header.getKey(), header.getValue());
            }
            for (Map.Entry<String, String> entry : param.entrySet()) {
                builder.add(entry.getKey(), entry.getValue());
            }
            Request request = requestBuilder.build();
            Response response = getInstance().newCall(request).execute();
            String result = response.body().string();
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }


    /**
     * get请求
     */
    public static String get(String url) {
        return get(url, Collections.EMPTY_MAP);
    }

    /**
     * get请求
     *
     * @param url     接口路径
     * @param headers 请求头
     */
    public static String get(String url, Map<String, String> headers) {
        try {
            URL realUrl = new URL(url);
            Request.Builder builder = new Request.Builder().url(realUrl).get();
            for (Map.Entry<String, String> header : headers.entrySet()) {
                builder.addHeader(header.getKey(), header.getValue());
            }
            Request request = builder.build();
            Response response = getInstance().newCall(request).execute();
            String result = response.body().string();
            return result;
        } catch (Exception e) {
            return null;
        }
    }

    public static OkHttpClient getInstance() {
        //设置连接,写入,读取超时时间
        return LazyLoadHttpClient.INSTANCE;
    }

    public static void main(String[] args) {
        HashMap<String, String> header = new HashMap<>();
        header.put("language"," en_US");
        JSONObject object = new JSONObject();
        object.put("email", "tiktokguild@gmail.com");
        object.put("password", "123456");
        object.put("systemName", "ios");
        HttpResult<String> result = HttpUtil.postJson("http://47.242.107.138:9090/home/login", header, object.toJSONString());
    }

    private static class LazyLoadHttpClient {

        /**
         * 连接超时时间
         */
        private final static int DEFAULT_CONNECT_TIMEOUT = 15;

        /**
         * 写入超时时间
         */
        private final static int DEFAULT_WRITE_TIMEOUT = 30;

        /**
         * 读取超时时间
         */
        private final static int DEFAULT_READ_TIMEOUT = 60;

        /**
         * 复用连接池
         */
        private static final ConnectionPool CONNECTION_POOL = new ConnectionPool(256, 5L, TimeUnit.MINUTES);

        private static OkHttpClient INSTANCE = new OkHttpClient.Builder()
                .connectTimeout(DEFAULT_CONNECT_TIMEOUT, TimeUnit.SECONDS)
                .writeTimeout(DEFAULT_WRITE_TIMEOUT, TimeUnit.SECONDS)
                .readTimeout(DEFAULT_READ_TIMEOUT, TimeUnit.SECONDS)
                .connectionPool(CONNECTION_POOL)
                .build();
    }


}


