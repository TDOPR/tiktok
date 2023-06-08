package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;

/**
 * @Description
 * @Author Dominick Li
 * @CreateTime 2023/3/26 16:58
 **/
@AllArgsConstructor
@Getter
public enum AmountRegionEunm {

    ONE("0-1000", new BigDecimal("1000")),
    TWO("1001-5000", new BigDecimal("5000")),
    THREE("5001-10000", new BigDecimal("10000")),
    FOUR("10001-20000", new BigDecimal("20000")),
    FIVE("20001-50000", new BigDecimal("50000")),
    SIX("50000以上", new BigDecimal("0")),
    ;
    private String name;

    private BigDecimal le;

    public static AmountRegionEunm getByAmountGe(BigDecimal amount) {
        for (AmountRegionEunm amountRegionEunm : values()) {
            if (amountRegionEunm.getLe().compareTo(amount) >= 0) {
                return amountRegionEunm;
            }
        }
        return SIX;
    }

}
