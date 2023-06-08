package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 提现审核列表查询条件
 * @CreateTime 2022/11/11 12:06
 **/
@Data
public class WalletsUsdWithdrawCondition extends BaseCondition<LocalDateTime> {

    /**
     * 用户邮箱号
     */
    private Integer userId;

    /**
     * 渠道类型
     */
    private Integer channelType;

    /**
     * 审核状态 ''=查所有 0=待审核 2=审核通过 3=驳回
     */
    private Integer status;

    @Override
    public QueryWrapper buildQueryParam() {
        return null;
    }
}
