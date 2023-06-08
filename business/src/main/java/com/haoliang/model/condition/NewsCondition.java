package com.haoliang.model.condition;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.model.News;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/29 18:00
 **/
@Data
public class NewsCondition  extends BaseCondition<LocalDateTime> {

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
     * 类型
     */
    private Integer type;

    @Override
    public QueryWrapper buildQueryParam() {
        return null;
    }
}
