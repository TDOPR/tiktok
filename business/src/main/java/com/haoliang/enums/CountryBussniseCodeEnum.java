package com.haoliang.enums;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum CountryBussniseCodeEnum {

    THAILAND("泰国","Thailand", "100029"),
    VIET_NAM("越南", "Viet Nam","100013"),
    INDONESIA("印度尼西亚", "Indonesia","100034");

    private String name;

    private String key;

    private String code;

    public String getCode() {
        return code;
    }

}
