package com.haoliang.model.condition;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 查询条件
 * @CreateTime 2023/2/27 12:06
 **/
@Data
public class AppUserTaskCondition {

    /**
     * 任务状态  1=已接单 2=审核中 3=已驳回 4=完成
     */
    private Integer status=1;

}
