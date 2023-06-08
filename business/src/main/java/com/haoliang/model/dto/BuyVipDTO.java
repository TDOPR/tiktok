package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/13 17:36
 **/
@Data
public class BuyVipDTO {

    private Integer level;

    /**
     * 可抵扣金额
     */
    private BigDecimal deductionsAmount;

    /**
     * 实际购买金额
     */
    private BigDecimal amount;


}
