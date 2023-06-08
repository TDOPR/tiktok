package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.NotEmpty;

@Data
public class LoginDTO {

    /**
     * 用户名
     */
    @NotEmpty
    private String username;

    /**
     * 密码
     */
    @NotEmpty
    private String password;

    /**
     * 验证码id 可选
     */
    private String uuid;

    /**
     * 验证码填写内容
     */
    private String code;

}
