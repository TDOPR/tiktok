package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import jodd.util.StringUtil;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/4 16:10
 **/
@Data
public class SysLoginLogCondition extends BaseCondition {

    /**
     * 用户名
     */
    private String username;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(username)) {
            username = username.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("username", username);
        }
        return this.getQueryWrapper();
    }
}
