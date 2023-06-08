package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.util.StringUtil;
import com.haoliang.model.AppVersions;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/9 11:33
 **/
@Data
public class AppVersionsCondition extends BaseCondition<LocalDateTime> {

    /**
     * 系统名称
     */
    private String systemName;

    /**
     * 版本号
     */
    private String version;

    /**
     * 激活状态   1=最新版本  0=旧版本
     */
    private Integer active;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(systemName)) {
            this.getQueryWrapper().eq("systemName", systemName);
        }
        if (StringUtil.isNotBlank(version)) {
            version = version.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("version", version);
        }

        if (active != null) {
            this.getQueryWrapper().eq("active", active);
        }
        return this.getQueryWrapper();
    }
}
