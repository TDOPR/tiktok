package com.haoliang.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;

/**
 * 代数奖励比例
 */
@Getter
@AllArgsConstructor
public enum AlgebraEnum {

    FIRST(1, new BigDecimal("0.20")),
    SECOND(2, new BigDecimal("0.10")),
    THIRD(3, new BigDecimal("0.03"));

    /**
     * 代数
     */
    private Integer level;

    /**
     * 奖励比例
     */
    private BigDecimal rewardProportion;

    public static BigDecimal getRechargeMaxByLevel(int level) {
        for (AlgebraEnum algebraEnum : values()) {
            if (level == algebraEnum.getLevel()) {
                return algebraEnum.getRewardProportion();
            }
        }
        return BigDecimal.ZERO;
    }
}
