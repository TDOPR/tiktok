package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.util.StringUtil;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 操作日志查询条件
 * @CreateTime 2022/11/4 16:10
 **/
@Data
public class SysOperationLogCondition extends BaseCondition {

    /**
     * 用户名 等值
     */
    private String username;

    /**
     * 模块 等值
     */
    private String module;
    /**
     * 操作 等值
     */
    private String description;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(username)) {
            this.getQueryWrapper().eq("username", username);
        }
        if (StringUtil.isNotBlank(module)) {
            this.getQueryWrapper().eq("module", module);
        }

        if (StringUtil.isNotBlank(description)) {
            this.getQueryWrapper().eq("description", description);
        }
        return this.getQueryWrapper();
    }
}
