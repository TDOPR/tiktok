package com.haoliang.enums;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum  SystemEnum {

    IOS("ios"),
    ANDROID("android");

    private String name;

    public String getName() {
        return name;
    }
}
