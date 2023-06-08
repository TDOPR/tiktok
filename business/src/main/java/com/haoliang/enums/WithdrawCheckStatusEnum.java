package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 提现状态枚举
 */
@AllArgsConstructor
@Getter
public enum WithdrawCheckStatusEnum {

    SUCCESS(0,"成功"),
    UNDER_REVIEW(1,"审核中"),
    REJECT(2,"驳回"),
    QUASH(3,"审核驳回撤销状态")
    ;

    private Integer status;
    private String desc;

}
