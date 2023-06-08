package com.haoliang.pay.id;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.enums.ReturnMessageEnum;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.util.*;
import com.haoliang.common.util.encrypt.RsaUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.pay.enums.PayStatusEnum;
import com.haoliang.pay.id.model.IdPayResult;
import com.haoliang.pay.id.model.IdProxyPayResult;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;

/**
 * @author Dominick Li
 * @Description 印尼法币支付工具类
 * @CreateTime 2023/4/14 15:21
 **/
@Slf4j
@NoArgsConstructor
public class IdPayUtils {

    /**
     * 成功标识
     */
    private static final String SUCCESS_FLAG = "SUCCESS";

    /**
     * 支付
     *
     * @param localDateTime 流水时间
     * @param orderNum      订单号
     * @param payMoney      支付金额
     * @param phone         手机号
     * @param email         邮箱号
     * @return
     */
    public static HttpResult<IdPayResult> pay(LocalDateTime localDateTime, String orderNum, String payMoney, String phone, String email) {
        String date = DateUtil.format(localDateTime, DateUtil.SIMPLE_DATETIME_NUMBER);
        Map<String, String> requestParams = new TreeMap<>();
        requestParams.put("merchantCode", IdPayConstants.MCH_ID);
        // 订单类型（0-法币交易）
        requestParams.put("orderType", "0");
        // 商户订单号
        requestParams.put("orderNum", orderNum);
        // 订单金额  最低1万
        requestParams.put("payMoney", payMoney);
        // 客户名称
        requestParams.put("name", TiktokConfig.ID_PAY_NAME);
        if (StringUtil.isBlank(phone)) {
            phone = "default";
        }
        // 手机号
        requestParams.put("phone", phone);
        // 客户邮箱
        requestParams.put("email", email);
        // 回调地址
        requestParams.put("notifyUrl", GlobalProperties.getCallBackUrl() + IdPayConstants.PAY_NOTIFY);
        // 时间戳 格式 yyyyMMddHHmmss
        requestParams.put("dateTime", date);
        // 过期时间 单位(分)
        requestParams.put("expiryPeriod", IdPayConstants.EXPIRY_PERIOD.toString());
        // 订单详情
        requestParams.put("productDetail", "Test Pay");

        // 拼接参数
        StringBuilder stringBuilder = new StringBuilder();
        for (String key : requestParams.keySet()) {
            stringBuilder.append(requestParams.get(key));
        }

        try {
            // 得到待加密的字符串
            String keyStr = stringBuilder.toString();
            System.out.println("keyStr:" + keyStr);
            //参数签名
            String signedStr = FinPayRequestUtil.privateEncrypt(keyStr, FinPayRequestUtil.getPrivateKey(IdPayConstants.PRIVATE_KEY));  // 私钥加密
            requestParams.put("sign", signedStr);
            log.info("印尼代收订单号:{} Post Json Params:{}", orderNum, requestParams);
            //代收暂时没有签名数据
            HttpResult<IdPayResult> httpResult = HttpUtil.postJson(IdPayConstants.PAY_URL, JSONObject.toJSONString(requestParams), IdPayResult.class);
            if (httpResult.isSuccess()) {
                if (!SUCCESS_FLAG.equals(httpResult.getData().getPlatRespCode())) {
                    httpResult.setSuccess(false);
                    httpResult.setMsg(httpResult.getData().getPlatRespMessage());
                }
            }
            return httpResult;
        } catch (Exception e) {
            log.error("印尼代收接口异常:{}", e.getMessage());
            return HttpResult.failureResult(e.getMessage());
        }
    }

    /**
     * 印尼代付
     *
     * @param orderNum      订单号
     * @param localDateTime 流水时间
     * @param payMoney      支付金额
     * @param bankCode      银行编码
     * @param bankNumber    银行卡号
     * @param name          持卡人姓名
     * @param phone         手机号
     * @param email         邮箱号
     * @return
     */
    public static HttpResult<IdProxyPayResult> proxyPay(String orderNum, LocalDateTime localDateTime, String payMoney, String bankCode, String bankNumber, String name, String phone, String email) {
        String date = DateUtil.format(localDateTime, DateUtil.SIMPLE_DATETIME_NUMBER);
        //String orderNum = IdUtil.getSnowflakeNextIdStr();
        Map<String, String> requestParams = new TreeMap<>();
        requestParams.put("merchantCode", IdPayConstants.MCH_ID);
        // 商户订单号
        requestParams.put("orderNum", orderNum);
        // 收款方式（Transfer）
        requestParams.put("method", "Transfer");
        // 订单类型（0-法币交易，1-数字货币交易）
        requestParams.put("orderType", "0");
        // 订单金额,不支持小数位
        requestParams.put("money", payMoney);
        //  手续费类型（0-帐内扣除，1-帐外扣除）
        requestParams.put("feeType", "1");
        // 时间戳 格式 yyyyMMddHHmmss
        requestParams.put("dateTime", date);

//        if (!GlobalProperties.isProdEnv()) {
//            //测试信息 TODO
//            bankCode = "014"; //BCA
//            bankNumber = "4850412265";
//            name = "Elvira";
//        }

        // 客户银行卡号
        requestParams.put("number", bankNumber);
        // 印尼银行编码：参考附录I 代付
        requestParams.put("bankCode", bankCode);
        // 持卡人真实姓名
        requestParams.put("name", name);
        if (StringUtil.isBlank(phone)) {
            phone = "default";
        }
        // 客户手机号
        requestParams.put("mobile", phone);
        // 客户邮箱
        requestParams.put("email", email);
        // 描述
        requestParams.put("description", "test cash");
        // 回调地址
        requestParams.put("notifyUrl", GlobalProperties.getCallBackUrl() + IdPayConstants.PROXY_PAY_NOTIFY);

        // 拼接参数
        StringBuilder stringBuilder = new StringBuilder();
        for (String key : requestParams.keySet()) {
            stringBuilder.append(requestParams.get(key));
        }

        try {
            // 得到待加密的字符串
            String keyStr = stringBuilder.toString();
            System.out.println("keyStr:" + keyStr);
            //参数签名 私钥加密
            String signedStr = FinPayRequestUtil.privateEncrypt(keyStr, FinPayRequestUtil.getPrivateKey(IdPayConstants.PRIVATE_KEY));
            requestParams.put("sign", signedStr);

            String postJson = JSONObject.toJSONString(requestParams);
            log.info("印尼代付订单号:{} Post Json Params:{}", orderNum, postJson);
            HttpResult<IdProxyPayResult> httpResult = HttpUtil.postJson(IdPayConstants.PROXY_PAY_URL, postJson, IdProxyPayResult.class);
            if (httpResult.isSuccess()) {
                if (SUCCESS_FLAG.equals(httpResult.getData().getPlatRespCode())) {
                    //签名验证通过,执行正常的业务逻辑
                    boolean pass = verifySign(BeanMapUtil.beanToTreeMap(httpResult.getData()));
                    if (pass) {
                        return httpResult;
                    }
                    httpResult.setMsg("验签失败！");
                } else {
                    httpResult.setMsg(httpResult.getData().getPlatRespMessage());
                }
            }
            httpResult.setSuccess(false);
            return httpResult;
        } catch (Exception e) {
            log.error("印尼代付接口异常:{} ", e);
            return HttpResult.failureResult(ReturnMessageEnum.ERROR.getKey());
        }
    }

    /**
     * 代收订单查询
     */
    public static HttpResult<Integer> queryPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, "ORDER_QUERY");
    }

    /**
     * 代付订单查询
     */
    public static HttpResult<Integer> queryProxyPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, "CASH_QUERY");
    }


    /**
     * 订单查询
     *
     * @param orderNum  商户订单号
     * @param queryType 订单类型
     * @return
     */
    private static HttpResult<Integer> queryOrderStatus(String orderNum, String queryType) {
        Map<String, String> requestParams = new TreeMap<>();
        requestParams.put("merchantCode", IdPayConstants.MCH_ID);
        requestParams.put("queryType", queryType);
        requestParams.put("orderNum", orderNum);
        requestParams.put("dateTime", "20220101105500");

        StringBuilder stringBuilder = new StringBuilder();
        for (String key : requestParams.keySet()) {
            stringBuilder.append(requestParams.get(key));  // 拼接参数
        }
        String keyStr = stringBuilder.toString();  // 得到待加密的字符串
        System.out.println("keyStr:" + keyStr);
        try {
            String signedStr = FinPayRequestUtil.privateEncrypt(keyStr, FinPayRequestUtil.getPrivateKey(IdPayConstants.PRIVATE_KEY));
            requestParams.put("sign", signedStr);

            String postJson = JSONObject.toJSONString(requestParams);
            System.out.println("Post Json Params:" + postJson);

            HttpResult<String> res = HttpUtil.postJson(IdPayConstants.QUERY_ORDER_URL, postJson);
            if (res.isSuccess()) {
                JSONObject object = JSONObject.parseObject(res.getData());
                if (object.getBoolean("success") && object.getInteger("code") == 1000) {
                    Integer status = null;
                    JSONObject data = object.getJSONObject("data");
                    if (queryType.equals("ORDER_QUERY")) {
                        //代收订单
                        status = PayStatusEnum.valueOf(data.getString("status")).getStatus();
                    } else {
                        //代付订单
                        status = data.getInteger("status");
                    }
                    return HttpResult.successResult(status);
                }
            }
            return HttpResult.failureResult(ReturnMessageEnum.ERROR.getKey());
        } catch (Exception e) {
            return HttpResult.failureResult(ReturnMessageEnum.ERROR.getKey());
        }
    }

    public static boolean verifySign(TreeMap<String, Object> treeMap) {
        String platSign = treeMap.remove("platSign").toString();
        StringBuilder stringBuilder = new StringBuilder();
        for (Object value : treeMap.values()) {
            stringBuilder.append(value);
        }
        String str = stringBuilder.toString();
        String decryptSign = RsaUtil.decryptByPublic(platSign, IdPayConstants.PLATFORM_PUBLIC_KEY);
        return str.equals(decryptSign);
    }


    public static void main(String[] args) {
        //payTest();
        //proxyPayTest();
        HttpResult<Integer> httpResult = queryPayOrder("1648585065397583872");
        if (httpResult.isSuccess()) {
            System.out.println("代收订单状态:" + httpResult.getData());
        }
//        HttpResult<Integer> httpResult2 = queryProxyPayOrder("1648998263539716096");
//        if (httpResult2.isSuccess()) {
//            System.out.println("代收订单状态:" + httpResult2.getData());
//        }
    }

    /**
     * 代付
     */
    public static void payTest() {
        String orderNum = IdUtil.getSnowflakeNextIdStr();
        System.out.println(orderNum);
        //单笔费率 = 基础费用+ 代收金额%5的手续费   最低金额包含了手续费
        HttpResult<IdPayResult> result = pay(LocalDateTime.now(), orderNum, "10000", "666888", "ZhangSan@email.com");
        if (result.isSuccess()) {
            if ("SUCCESS".equals(result.getData().getPlatRespCode())) {
                System.out.println("请求代收接口成功");
                System.out.println("平台订单号:" + result.getData().getPlatOrderNum());
                System.out.println("收银台链接:" + result.getData().getUrl());
            } else {
                System.out.println("请求代收接口失败:" + result.getData().getPlatRespMessage());
            }
        } else {
            log.info("http接口请求失败");
        }
    }

    public static void proxyPayTest() {
        //16830  16837  最低金额没包含手续费
        HttpResult<IdProxyPayResult> result = proxyPay(IdUtil.getSnowflakeNextIdStr(), LocalDateTime.now(), String.valueOf(FiatTypeEnum.ID.getProxyMin()), null, null, null, "123", "1234");
        log.info("success={}", result.isSuccess());
        log.info("data={}", result.getData());
    }

}
