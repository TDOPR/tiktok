package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum EmailTypeEnum {

    REGISTER(1,"注册"),
    PAY_CHECK(2,"支付验证"),
    FIND_PASSWORD(3,"找回密码"),
    ;

    private Integer value;

    private String name;

}
