package com.haoliang.test;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.model.HttpResult;
import com.haoliang.common.util.HttpUtil;
import com.haoliang.common.util.SignUtil;
import com.haoliang.pay.vn.SftSignUtil;
import com.haoliang.enums.CountryTelephoneCode;

import java.util.LinkedHashMap;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/10 20:09
 **/
public class TestSms {

    public static void main(String[] args) {
        String msg = "[TikTok Guild] Please verify your device\n" +
                "Verification code: %s\n" +
                "Expires in %s minutes\n";
        CountryTelephoneCode telephoneCode = CountryTelephoneCode.VN;
        //System.out.println(String.format(msg, "999888", "3"));
        String sendMsg=String.format(msg, "999888", "3");
        //手机号  泰国TH   越南VN  马来西亚MS  印度尼西亚ID
        String mobile = telephoneCode.getTestPhone();
        LinkedHashMap<String,String> param = new LinkedHashMap();
        param.put("appkey", telephoneCode.getAppKey());
        param.put("appsecret", telephoneCode.getAppSecret());
        param.put("appcode", telephoneCode.getAppCode());
        param.put("phone", telephoneCode.getCode() +mobile);
        param.put("msg",sendMsg);
        System.out.println(sendMsg);
        String sign = SignUtil.sortData(param);
        String url = "http://47.242.85.7:9090/sms/batch/v2?" + sign;
        System.out.println(url);
        HttpResult<String> result = HttpUtil.postJson(url, null, String.class);
        boolean flag = false;
        if (result.isSuccess()) {
            JSONObject object = JSONObject.parseObject(result.getData());
            if ("00000".equals(object.getString("code"))) {
                JSONArray jr = object.getJSONArray("result");
                if ("00000".equals(jr.getJSONObject(0).get("status"))) {
                    flag = true;
                }
            }
        }
        System.out.println("请求成功:" + flag);
        System.out.println(result.getData());

    }
}
