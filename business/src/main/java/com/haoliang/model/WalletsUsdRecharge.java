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
 * @Description  充值记录表
 * @CreateTime 2023/4/3 17:07
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("wallets_usd_recharge")
public class WalletsUsdRecharge extends BaseModel {

    /**
     * 单号
     */
    private String txid;

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 提现渠道 1=区块链  2=法币
     */
    private Integer coinId;

    /**
     * 链的名称
     */
    private String coinType;

    /**
     * 充值金额
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
     * 充值状态
     */
    private Integer status;

    /**
     * 充币地址
     */
    private String address;

}
