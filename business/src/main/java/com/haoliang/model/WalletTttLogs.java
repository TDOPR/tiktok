package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelNoModifyTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description 用户钱包流水表
 * @CreateTime 2022/11/1 10:57
 **/
@Data
@Builder
@TableName("wallet_ttt_logs")
@NoArgsConstructor
@AllArgsConstructor
public class WalletTttLogs extends BaseModelNoModifyTime {

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 本次变动金额
     */
    private BigDecimal amount;

    /**
     * 收支类型 1=收入 2=支出
     */
    private Integer action;

    /**
     * 流水类型 对应FlowingTypeEnum枚举
     */
    private Integer type;

    /**
     * 类型 1=零撸 0=有效用户产生的
     */
    private Integer zero;

}
