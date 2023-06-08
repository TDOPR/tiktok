package com.haoliang.pay.th;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.util.HttpUtil;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.SignUtil;
import com.haoliang.pay.enums.PayStatusEnum;
import com.haoliang.pay.th.enums.ThPayStatusEnum;
import com.haoliang.pay.th.model.ThPayResult;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description 泰国支付
 * @CreateTime 2023/4/19 17:21
 * 测试商户编号:20
 * 测试商户秘钥:70e604c93a00a33824a9cd41ee8e5e47
 **/
@Slf4j
public class ThPayUtils {

    private static final String BASE_URL = "https://api.paythreescore.com";

    /**
     * 代收地址
     */
    private static final String PAY_URL = BASE_URL + "/api/order";

    /**
     * 代付地址
     */
    private static final String PROXY_PAY_URL = BASE_URL + "/api/withdraw";

    /**
     * 查询充值订单状态接口
     */
    private static final String QUERY_ORDER_URL = BASE_URL + "/api/queryOrder";

    /**
     * 查询提现订单状态接口
     */
    private static final String QUERY_WITHDRAW_URL = BASE_URL + "/api/queryWithdraw";

    /**
     * 代收回调地址
     */
    private static final String PAY_NOTIFY = "/recharge/thCallBack";

    /**
     * 代收回调地址
     */
    private static final String PROXY_PAY_NOTIFY = "/withdraw/thCallBack";

    /**
     * 商户号
     */
    private static final String MCH_ID = "69";

    /**
     * 商户的私钥
     */
    private static final String MCH_KEY = "513b3855e83c33213256cecc44f859cf";

    /**
     * 代付
     *
     * @param orderNum 订单编号
     * @param payMoney 代收金额
     * @return
     */
    public static HttpResult<ThPayResult> pay(String orderNum, String payMoney) {
        Map<String, String> param = new HashMap<>();
        param.put("mch_id", MCH_ID);
        param.put("order_no", orderNum);
        param.put("trade_amount", payMoney);
        param.put("notify_url",GlobalProperties.getCallBackUrl() + PAY_NOTIFY);
        String signData = SignUtil.sortData(param);
        String sign = SignUtil.sign(signData, MCH_KEY);
        param.put("sign", sign);
        String postJson = JSONObject.toJSONString(param);
        log.info("泰国代收订单号:{} Post Json Params:{}", orderNum, param);
        HttpResult<ThPayResult> httpResult = HttpUtil.postJson(PAY_URL, postJson, ThPayResult.class);
        if (httpResult.isSuccess()) {
            if (HttpStatus.OK.value() != httpResult.getData().getState()) {
                httpResult.setSuccess(false);
                httpResult.setMsg(httpResult.getData().getMsg());
            }
        }
        return httpResult;
    }

    /**
     * 代收
     *
     * @param orderNum    订单编号
     * @param payMoney    代收金额
     * @param name        持卡人姓名
     * @param bank_code   银行编码
     * @param bank_number 银行卡号
     * @return
     */
    public static HttpResult<ThPayResult> proxyPay(String orderNum, String payMoney, String bank_code, String bank_number, String name) {
        Map<String, Object> param = new HashMap<>();
        param.put("mch_id", MCH_ID);
        param.put("order_no", orderNum);
        param.put("trade_amount", payMoney);
        param.put("notify_url", GlobalProperties.getCallBackUrl() + PROXY_PAY_NOTIFY);
        param.put("bank_user", name);
        param.put("bank_code", Integer.parseInt(bank_code));
        param.put("bank_number", bank_number);

        String signData = SignUtil.sortData(param);
        String sign = SignUtil.sign(signData, MCH_KEY);
        param.put("sign", sign);
        log.info("泰国代收订单号:{} Post Json Params:{}", orderNum, param);
        HttpResult<ThPayResult> httpResult = HttpUtil.postJson(PROXY_PAY_URL, JSONObject.toJSONString(param), ThPayResult.class);
        if (httpResult.isSuccess()) {
            if (HttpStatus.OK.value() != httpResult.getData().getState()) {
                httpResult.setSuccess(false);
                httpResult.setMsg(httpResult.getData().getMsg());
            }
        }
        return httpResult;
    }

    /**
     * 支付回调参数验签
     */
    public static boolean verifySign(Map<String, String> paramMap) {
        String resSign = paramMap.remove("sign");
        paramMap.remove("order_id");
        paramMap.remove("message");
        String signData = SignUtil.sortData(paramMap);
        return SignUtil.validateSignByKey(signData, MCH_KEY, resSign);
    }

    /**
     * 代收订单查询
     *
     * @Param orderNum 订单号
     */
    public static HttpResult<Integer> queryPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, QUERY_ORDER_URL);
    }

    /**
     * 代付订单查询
     *
     * @Param orderNum 订单号
     */
    public static HttpResult<Integer> queryProxyPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, QUERY_WITHDRAW_URL);
    }

    private static HttpResult<Integer> queryOrderStatus(String orderNum, String queryUrl) {
        Map<String, Object> param = new HashMap<>();
        param.put("mch_id", MCH_ID);
        param.put("order_no", orderNum);
        param.put("timestamp", System.currentTimeMillis());
        String signData = SignUtil.sortData(param);
        String sign = SignUtil.sign(signData, MCH_KEY);
        param.put("sign", sign);
        HttpResult<String> httpResult = HttpUtil.postJson(queryUrl, JSONObject.toJSONString(param));
        if (httpResult.isSuccess()) {
            JSONObject object = JSONObject.parseObject(httpResult.getData());
            if (object.getInteger("state").equals(HttpStatus.OK.value())) {
                JSONObject data = object.getJSONObject("data");
                Map<String, String> resParam = new HashMap<>();
                resParam.put("mch_id", data.getString("mch_id"));
                resParam.put("order_no", data.getString("order_no"));
                resParam.put("trade_amount", data.getString("trade_amount"));
                resParam.put("message", data.getString("message"));
                resParam.put("state", data.getString("state"));
                String resSignData = SignUtil.sortData(resParam);
                boolean pass = SignUtil.validateSignByKey(resSignData, MCH_KEY, data.getString("sign"));
                if (pass) {
                    return HttpResult.successResult(ThPayStatusEnum.nameOf(data.getInteger("state")).getStatus());
                }
                httpResult.setMsg("验签失败！");
            }
        }
        return HttpResult.failureResult(httpResult.getMsg());
    }

    /**
     * 代收测试
     */
    public static void payTest() {
        String orderNum = IdUtil.getSnowflakeNextIdStr();
        HttpResult<ThPayResult> result = pay(orderNum, "100");
        if (result.isSuccess()) {
            ThPayResult res = result.getData();
            System.out.println("支付地址=" + res.getData());
        } else {
            System.out.println("错误消息:" + result.getMsg());
        }
    }

    /**
     * 代付测试
     */
    public static void proxyPayTest() {
        String orderNum = IdUtil.getSnowflakeNextIdStr();
        HttpResult<ThPayResult> result = proxyPay(orderNum, "100", "101", "123456","张三");
        if (result.isSuccess()) {
            ThPayResult res = result.getData();
            System.out.println("代付请求成功=" + res);
        } else {
            System.out.println("错误消息:" + result.getMsg());
        }
    }

    public static void main(String[] args) {
        //payTest();
        proxyPayTest();
//        HttpResult<Integer> result = queryPayOrder("1648954085581635584");
//        if (result.isSuccess()) {
//            System.out.println(PayStatusEnum.stateOf(result.getData()).getName());
//        }
//        HttpResult<Integer> result1 = queryProxyPayOrder("1648954221795852288");
//        if (result1.isSuccess()) {
//            System.out.println(PayStatusEnum.stateOf(result1.getData()).getName());
//        }
    }


}
