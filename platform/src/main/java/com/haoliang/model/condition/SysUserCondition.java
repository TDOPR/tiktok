package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 操作日志查询条件
 * @CreateTime 2022/11/4 16:10
 **/
@Data
public class SysUserCondition extends BaseCondition {

    /**
     * 用户名 模糊
     */
    private String username;

    /**
     * 启用状态 1=正常,0=禁用，''=查全部
     */
    private Integer enabled;

    @Override
    public QueryWrapper buildQueryParam() {
        return null;
    }
}
