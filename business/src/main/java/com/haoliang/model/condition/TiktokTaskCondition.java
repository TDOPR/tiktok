package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.model.TiktokTask;
import jodd.util.StringUtil;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 查询还可以接单任务条件
 * @CreateTime 2023/2/24 14:22
 **/
@Data
public class TiktokTaskCondition extends BaseCondition<LocalDateTime> {

    /**
     * tiktok用户Id
     */
    private String userId;

    /**
     * tiktok用户名
     */
    private String userName;

    /**
     * 作品类型
     */
    private Integer type;

    @Override
    public QueryWrapper  buildQueryParam() {
        this.buildBaseQueryWrapper();
        this.getQueryWrapper().isNull("userId");
        if (StringUtil.isNotBlank(userId)) {
            this.getQueryWrapper().eq("tiktokUserId", userId);
        }
        if (type != null) {
            this.getQueryWrapper().eq("type", type);
        }
        if (StringUtil.isNotBlank(userName)) {
            this.getQueryWrapper().eq("userName", userName);
        }
        return this.getQueryWrapper();
    }
}
