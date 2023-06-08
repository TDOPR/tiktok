package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.constant.TiktokConfig;
import lombok.Data;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 汇率表
 * @CreateTime 2023/4/17 19:03
 **/
@Data
@TableName("currency_exchange_rates")
public class CurrencyExchangeRates {

    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;

    /**
     * 原货币
     */
    private String source;

    /**
     * 目标货币
     */
    private String target;

    /**
     * 源货币转目标货币的汇率
     */
    private BigDecimal exchangeRate;

    /**
     * 法币充值向上取整  多充钱
     *
     * @return
     */
    public BigDecimal getExchangeRateUp() {
        return this.exchangeRate.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.UP);
    }

    /**
     * 法币提现向下取整  少提钱
     *
     * @return
     */
    public BigDecimal getExchangeRateDown() {
        return this.exchangeRate.setScale(TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.DOWN);
    }

}
