package com.haoliang.model.dto;

import lombok.Data;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotEmpty;


@Data
public class AppUserLoginDTO {

    /**
     * 邮箱账号
     * @required
     */
    @NotEmpty
    @Email
    private String email;

    /**
     * 密码
     */
    private String password;

    /**
     * 系统名称
     */
    private String systemName;

}
