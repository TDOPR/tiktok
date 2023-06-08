//package com.haoliang.test;
//
//import com.haoliang.common.model.HttpResult;
//import com.haoliang.common.model.JsonResult;
//import lombok.AccessLevel;
//import lombok.NoArgsConstructor;
//import org.apache.poi.ss.formula.functions.T;
//import org.springframework.http.*;
//import org.springframework.util.LinkedMultiValueMap;
//import org.springframework.util.MultiValueMap;
//import org.springframework.web.client.RestClientException;
//import org.springframework.web.client.RestTemplate;
//
//import java.util.Map;
//
///**
// * @author Dominick Li
// * @CreateTime 2020/12/1 18:02
// * @description http工具类
// **/
//@NoArgsConstructor(access = AccessLevel.PRIVATE)
//public class RestTemplateTestUtil {
//
//
//    public static HttpResult get(String url) {
//        try {
//            ResponseEntity<String> entity = getClient().getForEntity(url, String.class);
//            if (entity.getStatusCode().value() == 200) {
//                return HttpResult.successResult(entity.getBody());
//            } else {
//                return HttpResult.failureResult(entity.getBody());
//            }
//        } catch (RestClientException e) {
//            e.printStackTrace();
//            return HttpResult.failureResult(e.getMessage());
//        }
//    }
//
//    public static HttpResult get(String url,Map<String,String> headers) {
//        try {
//            ResponseEntity<String> entity = getClient().getForEntity(url, String.class);
//            if (entity.getStatusCode().value() == 200) {
//                return HttpResult.successResult(entity.getBody());
//            } else {
//                return HttpResult.failureResult(entity.getBody());
//            }
//        } catch (RestClientException e) {
//            e.printStackTrace();
//            return HttpResult.failureResult(e.getMessage());
//        }
//    }
//
//
//
//    public static <T>HttpResult<T> postJson(String url, String jsonData,Class<T> tClass) {
//        HttpHeaders httpHeaders = new HttpHeaders();
//        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
//        HttpEntity<Object> httpEntity = new HttpEntity<>(jsonData, httpHeaders);
//        try {
//            ResponseEntity<T> entity = getClient().postForEntity(url, httpEntity, tClass);
//            if (entity.getStatusCode().value() == HttpStatus.OK.value()) {
//                System.out.println(entity.getBody());
//                return HttpResult.successResult(entity.getBody());
//            } else {
//                return HttpResult.failureResult(String.valueOf(entity.getStatusCodeValue()));
//            }
//        } catch (RestClientException e) {
//            return HttpResult.failureResult(e.getMessage());
//        }
//    }
//
//    public static HttpResult<T> post(String url, Map<String, Object> params,Class<T> tClass) {
//        HttpHeaders httpHeaders = new HttpHeaders();
//        httpHeaders.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
//        MultiValueMap<String, Object> multiValueMap = new LinkedMultiValueMap<>();
//        for (Map.Entry<String, Object> entry : params.entrySet()) {
//            multiValueMap.add(entry.getKey(), entry.getValue().toString());
//        }
//        HttpEntity<Object> httpEntity = new HttpEntity<>(multiValueMap, httpHeaders);
//        try {
//            ResponseEntity<T> entity = getClient().postForEntity(url, httpEntity, tClass);
//            if (entity.getStatusCode().value() == HttpStatus.OK.value()) {
//                return HttpResult.successResult(entity.getBody());
//            } else {
//                return HttpResult.failureResult(String.valueOf(entity.getStatusCodeValue()));
//            }
//        } catch (RestClientException e) {
//            e.printStackTrace();
//            return HttpResult.failureResult(e.getMessage());
//        }
//    }
//
//    public static RestTemplate getClient() {
//        return Lazy.CLIENT;
//    }
//
//    /**
//     * 使用懒加载方式实例化
//     */
//    private static class Lazy {
//        private static final RestTemplate CLIENT = new RestTemplate();
//    }
//
//}
