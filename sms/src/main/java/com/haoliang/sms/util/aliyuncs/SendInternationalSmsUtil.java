package com.haoliang.sms.util.aliyuncs;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.CommonRequest;
import com.aliyuncs.CommonResponse;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.exceptions.ServerException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;
import com.haoliang.sms.constant.ALiYunSmsConfig;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Dominick Li
 * @Description 发送国际短信
 * @CreateTime 2023/3/14 11:55
 **/
@Slf4j
public class SendInternationalSmsUtil {


    /**
     * 发送短信
     *
     * @param mobile 手机号
     * @param code   短信内容
     * @return 发送结果
     */
    public static boolean send(String mobile, String code) {
        CommonRequest request = new CommonRequest();
        request.setSysMethod(MethodType.POST);
        request.setSysDomain("dysmsapi.aliyuncs.com");
        request.setSysVersion("2017-05-25");
        request.setSysAction("SendMessageToGlobe");
        //接收号码，格式为：国际码+号码，必填
        request.putQueryParameter("To", mobile);
        request.putQueryParameter("Message", "");

        boolean flag=false;
        try {
            CommonResponse response = getClient().getCommonResponse(request);
            JSONObject object = JSONObject.parseObject(response.getData());
            if (object.getString("ResponseCode").equals("OK")) {
                return true;
            }
            System.out.println(response.getData());
        } catch (ServerException e) {
            log.info("send sms code ServerException:{}", e.getMessage());
        } catch (ClientException e) {
            log.info("send sms code ClientException:{}", e.getMessage());
        }
        return flag;
    }

    private static IAcsClient getClient() {
        return Lazy.acsClient;
    }

    /**
     * 懒加载
     */
    private static class Lazy {
        private static IAcsClient acsClient;

        static {
            DefaultProfile profile = DefaultProfile.getProfile(ALiYunSmsConfig.REGION_ID, ALiYunSmsConfig.ACCESS_KEY_ID, ALiYunSmsConfig.ACCESS_SECRET);
            Lazy.acsClient = new DefaultAcsClient(profile);
        }
    }

}
