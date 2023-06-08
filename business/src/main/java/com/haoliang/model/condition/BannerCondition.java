package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.model.Banner;
import jodd.util.StringUtil;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/9 10:24
 **/
@Data
public class BannerCondition extends BaseCondition<LocalDateTime> {

    /**
     * 图片名称
     */
    private String name;

    /**
     * 使用状态 1=显示 0=隐藏
     */
    private Integer enabled;

    /**
     * 国际化 zh_CN=中文;en_US=英语;in_ID=印度尼西亚文;th_TH=泰语;vi_VN=越南语
     */
    private String language;

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(name)) {
            name = name.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("name", name);
        }
        if (enabled!=null) {
            this.getQueryWrapper().eq("enabled", enabled);
        }
        return this.getQueryWrapper();
    }
}
