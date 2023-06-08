package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TaskStatusEnum {

    DEFAULT(1, "已接单"),
    TO_BE_CHECK(2, "审核中"),
    OVERRULE(3, "未通过"),
    SUCCESS(4, "通过"),
    ;

    private int code;

    private String name;

}
