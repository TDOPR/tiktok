package com.haoliang.sms.constant;

/**
 * @author Dominick Li
 * @Description 发送短信需要的
 * @CreateTime 2023/3/14 10:55
 **/
public interface ALiYunSmsConfig {

    /**
     * 认证的key
     */
    String ACCESS_KEY_ID ="";

    /**
     * 认证的Secret
     */
    String ACCESS_SECRET ="";

    /**
     * 区域Id
     */
    String REGION_ID ="cn-hangzhou";

    /**
     * 申请的签名名称
     */
    String SIGN_NAME ="阿里云短信测试";

    /**
     * 创建的短信模板code
     */
    String TEMPLATE_CODE ="SMS_154950909";

    /**
     * 模板中的占位符
     */
    String TEMPLATE_PARAM ="{\"code\":%s}";
}
