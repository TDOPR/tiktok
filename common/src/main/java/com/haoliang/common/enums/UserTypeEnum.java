package com.haoliang.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/22 17:28
 **/
@Getter
@AllArgsConstructor
public enum UserTypeEnum {

    SYSTEM(1,""),
    CLIENT(2,""),
    ;

    int value;

    String name;

    public int getValue() {
        return value;
    }

}


