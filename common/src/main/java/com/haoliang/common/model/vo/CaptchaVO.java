package com.haoliang.common.model.vo;

import lombok.Data;

/**
 * @Description
 * @Author Dominick Li
 **/
@Data
public class CaptchaVO {

    public CaptchaVO(String uuid, String captchaImagel) {
        this.uuid = uuid;
        this.captchaImagel = captchaImagel;
    }

    /**
     * 验证码Id
     */
    private String uuid;

    /**
     * 验证码base64数据
     */
    private String captchaImagel;

}
