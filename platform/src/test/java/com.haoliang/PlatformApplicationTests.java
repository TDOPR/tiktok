//package com.haoliang;
//
//
//import com.alibaba.fastjson.JSONObject;
//import com.haoliang.common.model.JsonResult;
//import com.haoliang.pay.collection.CollectionParam;
//import com.haoliang.pay.collection.OrderPayCallBack;
//import com.haoliang.test.RestTemplateTestUtil;
//import lombok.extern.slf4j.Slf4j;
//import org.junit.jupiter.api.Test;
//import org.springframework.boot.test.context.SpringBootTest;
//
//@Slf4j
//@SpringBootTest
//class PlatformApplicationTests {
//
//
//    @Test
//    public void contextLoads() throws Exception {
//        collection();
//    }
//
//    /**
//     * 代付下单
//     */
//    public static void collection() {
//        String URL = "https://api.qg-pay.com/orderPay";
//        CollectionParam collectionParam = CollectionParam.builder()
//                .merchant("100010")
//                .orderNo("1")
//                .businessCode("100010")
//                .name("test")
//                .pageUrl("15112345678")
//                .email("15112345678@163.com")
//                .amount("10000")
//                .notifyUrl("http://localhost:100/test")
//                .pageUrl("http://localhost:100/test")
//                .bankCode("10010")
//                .subject("测试")
//                .build();
//        String sign = "";
//        collectionParam.setSign(sign);
//        JsonResult<OrderPayCallBack> result = RestTemplateTestUtil.postJson(URL, JSONObject.toJSONString(collectionParam), OrderPayCallBack.class);
//        System.out.println(JSONObject.toJSONString(result));
//    }
//}
