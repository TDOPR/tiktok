package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/27 16:13
 **/
@Data
public class MobileDTO {

    /**
     * 手机号
     */
    private String mobile;

    /**
     * 验证码Id
     */
    private String uuid;

    /**
     * 短信验证码
     */
    private String code;

}
