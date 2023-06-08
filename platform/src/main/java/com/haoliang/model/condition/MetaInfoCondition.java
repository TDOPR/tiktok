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
public class MetaInfoCondition extends BaseCondition {

    /**
     * 模型名称
     */
    private String metaName;

    /**
     * 表名称
     */
    private String tableCode;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(metaName)) {
            this.getQueryWrapper().eq("metaName", metaName);
        }
        if (StringUtil.isNotBlank(tableCode)) {
            this.getQueryWrapper().eq("tableCode", tableCode);
        }
        return this.getQueryWrapper();
    }
}
