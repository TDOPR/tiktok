package com.haoliang.model.condition;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.model.TreatGuestDinner;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/23 17:19
 **/
@Data
public class TreatGuestDinnerCondition extends BaseCondition<LocalDateTime> {

    /**
     * 用户ID
     */
    private Integer userId;

    /**
     * 用户昵称
     */
    private String userName;

    /**
     * 审核状态  2=待审核 3=驳回  4=已完成
     */
    private Integer status;


    @Override
    public QueryWrapper buildQueryParam() {
        return null;
    }
}
