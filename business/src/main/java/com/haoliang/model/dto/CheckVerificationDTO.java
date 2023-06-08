package com.haoliang.model.dto;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/19 11:33
 **/
@Data
public class CheckVerificationDTO {

    /**
     * 验证码Id
     * @required
     */
    private String uuid;

    /**
     * 验证码
     * @required
     */
    private String code;
}
