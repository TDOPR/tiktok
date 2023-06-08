package com.haoliang.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum RoleTypeEnum {

    ADMIN(3, "admin"),
    PROXY(5,"proxy");

    private Integer id;
    private String code;

    public static RoleTypeEnum codeOf(String name) {
        for (RoleTypeEnum roleTypeEnum : values()) {
            if (roleTypeEnum.code.equals(name)) {
                return roleTypeEnum;
            }
        }
        return null;
    }

    public static RoleTypeEnum idOf(Integer val) {
        for (RoleTypeEnum roleTypeEnum : values()) {
            if (roleTypeEnum.id.equals(val)) {
                return roleTypeEnum;
            }
        }
        return null;
    }

}
