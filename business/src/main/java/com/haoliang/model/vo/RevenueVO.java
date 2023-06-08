package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/1/7 12:16
 **/
@Data
public class RevenueVO {

    /**
     * 用户代数收益
     */
    private String algebraReward;

    /**
     * 用户团队收益
     */
    private String teamReward;

    /**
     * 用户分红收益
     */
    private String superReward;

    /**
     * 用户动态收益总量
     */
    private String dynamicSumReward;

    /**
     * ttt兑换量
     */
    private String tttAmount;

    /**
     * 静态收益
     */
    private String staticSumReward;

    /**
     * 法币提现金额
     */
    private String fastAmount;

    /**
     * Usd提现金额
     */
    private String usdAmount;
}
