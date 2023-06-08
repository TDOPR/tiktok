package com.haoliang.model.vo;

import lombok.Data;

import java.math.BigDecimal;


/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 10:07
 **/
@Data
public class FiatRechargeInfoVO {

    /**
     * 法币类型  1=印尼 2=越南 3=泰国
     */
    private Integer type;

    /**
     * 费率
     */
    private String feeRate;

    /**
     * 单笔最小代收金额 10000
     */
    private Integer min;

    /**
     * 单笔最大代收金额
     */
    private Integer max;

    /**
     * 基础手续费   手续费=基础手续费+(充值金额 * 费率)  例如充值10000 则手续费=6500+10000*0.05=7000
     */
    private Integer baseFee;

    /**
     * 币种名称
     */
    private String coinName;

    /**
     * 币种单位
     */
    private String coinUnit;

    /**
     *  失效时间
     */
    private Integer  expiryPeriod;

    /**
     * USD 转 法币的汇率
     */
    private String exchangeRate;

    /**
     * 待支付的订单链接 不为空需要提示是否跳转到该链接
     */
    //private String url;

    /**
     * 订单号
     */
    //private String  orderNo;

}
