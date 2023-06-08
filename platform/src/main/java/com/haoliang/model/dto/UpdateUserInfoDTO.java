package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/3 14:50
 **/
@Data
public class UpdateUserInfoDTO {

    private String nickName;


    private String email;

    private String mobile;

    private String oldPassword;

    private String password;

}
