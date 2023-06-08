package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.enums.LanguageEnum;
import jodd.util.StringUtil;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/5/8 10:47
 **/
@Data
public class ArticleCondition extends BaseCondition<LocalDateTime> {

    /**
     * 标题
     */
    private String title;

    /**
     * 关键词
     */
    private String info;

    /**
     * 使用启用 1=启用 0=禁用
     */
    private Integer enabled;

    /**
     * 语种
     */
    private Integer language = LanguageEnum.ZH_CN.getType();

    @Override
    public QueryWrapper buildQueryParam() {
        this.buildBaseQueryWrapper();
        if (StringUtil.isNotBlank(title)) {
            title = title.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("title", title);
        }
        if (StringUtil.isNotBlank(info)) {
            info = info.replaceAll("%", "////%").replaceAll("_", "////_");
            this.getQueryWrapper().like("info", info);
        }
        if (enabled != null) {
            this.getQueryWrapper().eq("enabled", enabled);
        }
        this.getQueryWrapper().eq("language", language);
        return this.getQueryWrapper();
    }

}
