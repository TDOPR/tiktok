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
public class SysErrorLogCondition extends BaseCondition {

    /**
     * 错误信息 模糊
     */
    private String errorMsg;

    /**
     * ip地址 等值
     */
    private String ipAddr;

    /**
     * 错误类型 等值
     */
    private String errorType;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(errorMsg)) {
            errorMsg = errorMsg.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("errorMsg", errorMsg);
        }
        if (StringUtil.isNotBlank(ipAddr)) {
            this.getQueryWrapper().eq("ipAddr", ipAddr);
        }

        if (StringUtil.isNotBlank(errorType)) {
            this.getQueryWrapper().eq("errorType", errorType);
        }
        return this.getQueryWrapper();
    }
}
