package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/25 16:28
 **/
@Getter
@AllArgsConstructor
public enum ResponseStatusEnums {

    ZERO_LEVEL_USER_LIMIT_ERROR(201, "zero_level_user_limit_error", "未购买vip等级的用户必须直推三个有效用户才能提现！"),
    TASK_EXISTS_USER_USE(202, "task_exists_user_use", "选中的任务有%d条已被用户接单,是否强制删除数据？"),
    INSUFFICIENT_LIMIT_CODE(501, "insufficient_limit", "余额不足，请购买vip套餐后再接单！");

    private int code;

    private String msg;

    private String name;
}
