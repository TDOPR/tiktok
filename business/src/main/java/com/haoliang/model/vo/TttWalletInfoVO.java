package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/6 16:05
 **/
@Data
@Builder
@AllArgsConstructor
public class TttWalletInfoVO {

    /**
     * 历史总收益
     */
    private String historyEarningsTtt;

    /**
     * 历史总收益约等于的美元
     */
    private String historyEarningsUsd;

    /**
     * ttt余额
     */
    private String tttBalance;

    /**
     * ttt余额约等于的美元
     */
    private String tttBalanceUsd;

    /**
     * usd余额
     */
    private String usdBalance;

    /**
     * 汇率
     */
    private String exchangeRate;

}
