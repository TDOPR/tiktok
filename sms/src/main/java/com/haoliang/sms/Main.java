package com.haoliang.sms;

import com.aliyuncs.CommonRequest;
import com.aliyuncs.CommonResponse;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;


public class Main {
    public static void main(String[] args) throws Exception {
        //初始化acsClient，<accessKeyId>和<accessSecret>"在短信控制台查询即可
        DefaultProfile profile = DefaultProfile.getProfile("ap-southeast-1", "<accessKeyId>", "<accessSecret>");
        IAcsClient client = new DefaultAcsClient(profile);
        CommonRequest request = new CommonRequest();
        request.setSysMethod(MethodType.POST);
        //域名，请勿修改
        request.setSysDomain("dysmsapi.ap-southeast-1.aliyuncs.com");
        //API版本号，请勿修改
        request.setSysVersion("2018-05-01");
        //API名称
        request.setSysAction("SendMessageToGlobe");
        //接收号码，格式为：国际码+号码，必填
        request.putQueryParameter("To", "62123****8901");
        //发送方senderId，选填
        //request.putQueryParameter("From", "1234567890");
        //短信内容，必填
        request.putQueryParameter("Message", "have a test.");
        try {
            CommonResponse response = client.getCommonResponse(request);
            System.out.println(response.getData());
        } catch (ClientException e) {
            e.printStackTrace();
        }
    }
}