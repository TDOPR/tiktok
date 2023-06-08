package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.model.AppUsers;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 客户查询条件
 * @CreateTime 2022/11/11 18:52
 **/
@Data
public class AppUsersCondition extends BaseCondition<LocalDateTime> {

    /**
     * 用户Id
     */
    private Integer id;

    /**
     * 用户名
     */
    private String email;

    /**
     * 用户等级
     */
    private Integer level;

    /**
     * vip等级
     */
    private Integer vipLevel;

    /**
     * 是否有效
     */
    private Integer valid;

    /**
     * 代理商Id
     */
    private Integer proxyUserId;

    /**
     * 是否为代理商角色查询
     */
    private boolean proxy=false;

    /**
     * 代理商角色 1=是  0=不是
     */
    private Integer proxyRole;

    @Override
    public QueryWrapper buildQueryParam() {
        return null;
    }
}
