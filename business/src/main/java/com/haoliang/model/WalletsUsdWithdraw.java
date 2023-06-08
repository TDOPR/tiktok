package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 提现审核记录表
 * @CreateTime 2023/4/3 17:07
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("wallets_usd_withdraw")
public class WalletsUsdWithdraw extends BaseModel {

    /**
     * 单号
     */
    private Long txid;

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 提现渠道
     */
    private Integer coinId;

    /**
     * 链的名称
     */
    private String coinType;

    /**
     * 提现金额
     */
    private BigDecimal amount;

    /**
     * 费率
     */
    private BigDecimal free;

    /**
     * 实际提现金额
     */
    private BigDecimal actualAmount;

    /**
     * 审核状态  0=待审核 1=自动审核 2=驳回 4=审核通过
     */
    private Integer status;

    /**
     * 审核时间
     */
    private LocalDateTime auditTime;

    /**
     * 提币地址
     */
    private String address;

    /**
     * 流水Id
     */
    private Long usdLogsId;

}
