package com.haoliang.model.dto;

import lombok.Data;


@Data
public class AppUserRegisterDTO extends FindPasswordDTO {

    /**
     * 邀请码
     */
    private String inviteCode;


}
