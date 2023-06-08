package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description 充值表
 * @CreateTime 2023/4/15 11:12
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("fiat_recharge_order")
public class FiatRechargeOrder extends BaseModel {

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 法币类型  1=印尼 2=越南 3=泰国
     */
    private Integer fiatType;

    /**
     * 平台订单
     */
    private String platformOrderNo;

    /**
     * 支付链接
     */
    private String url;

    /**
     * 支付状态 2=未支付,3=支付成功,4=撤销,5=支付失败
     */
    private Integer status;

    /**
     * 充值金额 法币
     */
    private Integer amount;

    /**
     * 手续费 法币
     */
    private BigDecimal fee;

    /**
     * 实际到账 法币
     */
    private BigDecimal actualAmount;

    /**
     * 转成USDT后的金额
     */
    private BigDecimal usdAmount;

    /**
     * 汇率
     */
    private BigDecimal exchangeRate;

}
