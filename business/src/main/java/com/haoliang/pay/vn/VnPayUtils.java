package com.haoliang.pay.vn;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.config.GlobalProperties;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.util.BeanMapUtil;
import com.haoliang.common.util.HttpUtil;
import com.haoliang.common.util.IdUtil;
import com.haoliang.common.util.SignUtil;
import com.haoliang.pay.vn.enums.VnBankCodeEnum;
import com.haoliang.pay.vn.enums.VnPayStatusEnum;
import com.haoliang.pay.vn.model.VnPayResult;
import com.haoliang.pay.vn.model.VnPayStatusResult;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Dominick Li
 * @Description 越南支付
 * @CreateTime 2023/4/18 10:40
 **/
@Slf4j
@NoArgsConstructor
public class VnPayUtils {

    /**
     * 接口地址
     */
    private static final String API_URL = "http://Lidapay.net:2024/api";

    /**
     * MD5签名的key
     */
    public static final String SIGN_KEY = "0254563235284b28bd522ba4da51497e";

    /**
     * 代收回调地址
     */
    private static final String PAY_NOTIFY = "/recharge/vnCallBack";

    /**
     * 代收回调地址
     */
    private static final String PROXY_PAY_NOTIFY = "/withdraw/vnCallBack";

    /**
     * 成功标识
     */
    private static final String SUCCESS_FLAG = "success";

    /**
     * 商户号
     */
    private static final String MERCHANT_ID = "API23777051326929450";

    /**
     * 代收通道
     */
    private static final String PAY_CHANNEL = "PayCreateOrder";

    /**
     * 代付通道
     */
    private static final String PROXY_PAY_CHANNEL = "WithdrawalOrder";

    /**
     * 代收接口
     *
     * @param orderNum 订单号
     * @param payMoney 代收金额
     * @return
     */
    public static HttpResult<VnPayResult> pay(String orderNum, String payMoney) {
        Map<String, String> param = new HashMap<>();
        param.put("service", PAY_CHANNEL);
        param.put("merchant_id", MERCHANT_ID);
        param.put("nonce_str", orderNum);
        param.put("notify_url", GlobalProperties.getCallBackUrl() + PAY_NOTIFY);
        param.put("return_url", "");
        param.put("order_no", orderNum);
        param.put("biz_amt", payMoney);
        //通道類型對應的ID編號 105=个人银行
        param.put("biz_type", "105");
        //银行名称
        //param.put("bankBranchName", bankBranchName);
        try {
            String sortData = SftSignUtil.formatSignData(param);
            String sign = SftSignUtil.sign(sortData, SIGN_KEY);
            param.put("sign", sign);
            log.info("越南代付订单号:{} Post Json Params:{}", orderNum, param);
            HttpResult<VnPayResult> result = HttpUtil.postJson(API_URL, JSONObject.toJSONString(param), VnPayResult.class);
            if (result.isSuccess()) {
                if (SUCCESS_FLAG.equals(result.getData().getResult())) {
                    boolean pass = VnPayUtils.verifySign(BeanMapUtil.beanToStrMap(result.getData()));
                    if (pass) {
                        return result;
                    }
                    result.setMsg("验签失败!");
                } else {
                    result.setMsg(result.getData().getMessage());
                }
            }
            result.setSuccess(false);
            return result;
        } catch (Exception e) {
            log.error("越南代收接口异常:{}", e.getMessage());
            return HttpResult.failureResult(e.getMessage());
        }
    }


    /**
     * 代付
     *
     * @param orderNum   订单号
     * @param payMoney   支付金额
     * @param bankCode   银行编码
     * @param bankNumber 卡号
     * @param name       持卡人姓名
     * @return
     */
    public static HttpResult<VnPayResult> proxyPay(String orderNum, String payMoney, String bankCode, String bankNumber, String name) {
        Map<String, String> requestMap = new HashMap();
        requestMap.put("service", PROXY_PAY_CHANNEL);
        requestMap.put("merchant_id", MERCHANT_ID);
        requestMap.put("biz_type", "105");
        requestMap.put("nonce_str", orderNum);
        requestMap.put("order_no", orderNum);
        requestMap.put("biz_amt", payMoney);
        requestMap.put("notify_url", GlobalProperties.getCallBackUrl() + PROXY_PAY_NOTIFY);
        requestMap.put("bankCode", bankCode);
        requestMap.put("accName", name);
        requestMap.put("cardNo", bankNumber);
        try {
            String signStr = SftSignUtil.formatSignData(requestMap);
            System.out.println("参数：" + JSON.toJSONString(requestMap));
            System.out.println("signStr签名：" + signStr);
            String sign = SftSignUtil.sign(signStr, SIGN_KEY);
            requestMap.put("sign", sign);
            HttpResult<VnPayResult> result = HttpUtil.postJson(API_URL, JSONObject.toJSONString(requestMap), VnPayResult.class);
            if (result.isSuccess()) {
                if (SUCCESS_FLAG.equals(result.getData().getResult())) {
                    boolean pass = VnPayUtils.verifySign(BeanMapUtil.beanToStrMap(result.getData()));
                    if (pass) {
                        return result;
                    }
                    result.setMsg("验签失败！");
                } else {
                    result.setMsg(result.getData().getMessage());
                }
            }
            result.setSuccess(false);
            return result;
        } catch (Exception e) {
            log.error("越南代付接口异常:{}", e.getMessage());
            return HttpResult.failureResult(e.getMessage());
        }
    }

    /**
     * 签名验证
     */
    public static boolean verifySign(Map<String, String> beanToStrMap) {
        //支付平台返回的签名
        String resSign = beanToStrMap.remove("sign");
        String signData = SftSignUtil.formatSignData(beanToStrMap);
        return SignUtil.validateSignByKey(signData, SIGN_KEY, resSign);
    }


    /**
     * 代收订单查询
     */
    public static HttpResult<Integer> queryPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, "OrderQuery");
    }

    /**
     * 代付订单查询
     */
    public static HttpResult<Integer> queryProxyPayOrder(String orderNum) {
        return queryOrderStatus(orderNum, "OrderQuery");
    }

    /**
     * 订单查询
     *
     * @param orderNum    商户订单号
     * @param channelType 通道类型
     * @return
     */
    private static HttpResult<Integer> queryOrderStatus(String orderNum, String channelType) {
        Map<String, String> param = new HashMap<>();
        param.put("service", channelType);
        param.put("merchant_id", MERCHANT_ID);
        param.put("nonce_str", orderNum);
        param.put("order_no", orderNum);
        try {
            String sortData = SftSignUtil.formatSignData(param);
            String sign = SftSignUtil.sign(sortData, SIGN_KEY);
            param.put("sign", sign);
            HttpResult<VnPayStatusResult> result = HttpUtil.postJson(API_URL, JSONObject.toJSONString(param), VnPayStatusResult.class);
            if (result.isSuccess()) {
                VnPayStatusResult vnPayStatusResult = result.getData();
                if (SUCCESS_FLAG.equals(vnPayStatusResult.getResult())) {
                    boolean pass = VnPayUtils.verifySign(BeanMapUtil.beanToStrMap(result.getData()));
                    if (pass) {
                        return HttpResult.successResult(VnPayStatusEnum.nameOf(vnPayStatusResult.getStatus()).getStatus());
                    }
                    result.setSuccess(false);
                    result.setMsg("verifySign error!");
                } else {
                    result.setSuccess(false);
                    result.setMsg(result.getData().getMessage());
                }
            }
            return HttpResult.failureResult(result.getMsg());
        } catch (Exception e) {
            log.error("调用越南查询订单接口异常:{}", e.getMessage());
            return HttpResult.failureResult(e.getMessage());
        }
    }


    public static void main(String[] args) {
        String orderNum = IdUtil.getSnowflakeNextIdStr();
//        if (false) {
//            HttpResult<VnPayResult> result = pay(orderNum, "100");
//            if (result.isSuccess()) {
//                System.out.println("请求成功,地址为\n" + result.getData().getUrl());
//            } else {
//                System.out.println(result.getMsg());
//            }
//        }
        if (true) {
            HttpResult<VnPayResult> result = proxyPay(orderNum, "100", VnBankCodeEnum.ACB.getCode(), "6230520080090841111", "test");
            if (result.isSuccess()) {
                System.out.println("代付请求成功!");
            }
        }
//        HttpResult<Integer> httpResult = queryPayOrder("1649309280308711424");
//        if(httpResult.isSuccess()){
//            System.out.println("代收订单状态:"+ PayStatusEnum.stateOf(httpResult.getData()).getName());
//        }
//        HttpResult<Integer> httpResult1 = queryProxyPayOrder("1649239282764034048");
//        if (httpResult1.isSuccess()) {
//            System.out.println("代付订单状态:" + PayStatusEnum.stateOf(httpResult1.getData()).getName());
//        }
    }

}
