package com.haoliang.common.model.dto;

import lombok.Data;

import javax.validation.constraints.NotEmpty;

/**
 * @author Dominick Li
 * @description 修改密码
 **/
@Data
public class UpdatePasswordDTO {

    /**
     * 原密码
     */
    @NotEmpty
    private String oldPassword;

    /**
     * 新密码
     */
    @NotEmpty
    private String password;

}
