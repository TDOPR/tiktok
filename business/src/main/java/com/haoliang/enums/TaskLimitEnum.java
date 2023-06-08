package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TaskLimitEnum {

    //代理商等级基于用户小团队业绩总和去判断是否有资格升级
    ZERO(0, 5),
    ONE(1, 5),
    TWO(2, 20),
    THREE(3, 30),
    FOUR(4, 40),
    FIVE(5, 50),
    ;

    /**
     * 代理商等级
     */
    private int level;

    /**
     * 每天能领取的任务上限
     */
    private int limit;

}
