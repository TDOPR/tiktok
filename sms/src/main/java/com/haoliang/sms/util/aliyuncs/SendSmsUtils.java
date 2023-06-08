package com.haoliang.sms.util.aliyuncs;

import com.haoliang.sms.enums.CountryTelephoneCode;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/14 11:53
 **/
@Slf4j
public class SendSmsUtils {

    /**
     * 发送短信
     *
     * @param mobile 手机号
     * @param code   短信内容
     * @return 发送结果
     */
    public static boolean send(String mobile, String code) {
        if (mobile.startsWith(CountryTelephoneCode.CN.getCode())) {
            log.info("发送国内短信");
            return SendTemplateSmsUtil.send(mobile, code);
        }
        log.info("发送国际短信");
        return SendInternationalSmsUtil.send(mobile, code);
    }

}
