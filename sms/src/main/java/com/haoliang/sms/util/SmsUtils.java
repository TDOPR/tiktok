package com.haoliang.sms.util;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.RestTemplateUtil;
import com.haoliang.sms.enums.CountryTelephoneCode;
import lombok.NoArgsConstructor;
import org.apache.http.HttpStatus;

import java.util.Map;
import java.util.TreeMap;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/27 9:40
 **/
@NoArgsConstructor
public class SmsUtils {

    /**
     * 短信平台接口地址
     */
    private static final String URL = "http://47.242.85.7:9090/sms/batch/v2?";

    /**
     * 短信模板内容
     */
    private static final String MSG = "[TikTok Guild] Please verify your device\n" +
            "Verification code: %s\n" +
            "Expires in %s minutes\n" +
            "Please use it as soon as possible!";

    /**
     * 发送短信
     *
     * @param countryTelephoneCode 国家地区编号
     * @param mobile               手机号
     * @param code                 短信内容
     * @return 发送结果
     */
    public static boolean send(String countryTelephoneCode, String mobile, String code, String times) {
        CountryTelephoneCode telephoneCode = CountryTelephoneCode.codeOf(countryTelephoneCode);
        if (telephoneCode == null) {
            return false;
        }
        TreeMap<String, String> param = new TreeMap();
        param.put("appkey", telephoneCode.getAppKey());
        param.put("appsecret", telephoneCode.getAppSecret());
        param.put("appcode", telephoneCode.getAppCode());
        param.put("phone", telephoneCode.getCode() + mobile);
        param.put("msg", String.format(MSG, code, times));
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, String> entry : param.entrySet()) {
            sb.append(entry).append("=").append(entry.getValue()).append("&");
        }
        sb.deleteCharAt(sb.length() - 1);
        String sign = sb.toString();
        String url = URL + sign;
        System.out.println(url);
        if(true){
            return true;
        }
        JsonResult<String> result = RestTemplateUtil.postJson(url, null);
        boolean flag = false;
        if (result.getCode() == HttpStatus.SC_OK) {
            JSONObject object = JSONObject.parseObject(result.getData());
            if ("00000".equals(object.getString("code"))) {
                JSONArray jr = object.getJSONArray("result");
                if ("00000".equals(jr.getJSONObject(0).get("status"))) {
                    flag = true;
                }
            }
        }
        return flag;
    }

    public static void main(String[] args) {
        System.out.println(SmsUtils.send(CountryTelephoneCode.TH.getCode(),"12345","888999","3"));
    }
}
