package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotEmpty;


@Data
public class FindPasswordDTO {

    /**
     * 邮箱账号
     * @required
     */
    @NotEmpty
    @Email
    private String email;

    /**
     * 新密码
     * @required
     */
    @NotEmpty
    private String password;

    /**
     * 验证码Id
     * @required
     */
    @NotEmpty
    private String uuid;

    /**
     * 验证码
     * @required
     */
    @NotEmpty
    private String code;

}
