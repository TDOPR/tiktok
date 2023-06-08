package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/16 17:36
 **/
@Data
public class PlatformTotalLockAmountDTO {

    /**
     * 机器人总金额
     */
    private BigDecimal totalRobotAmount;

    /**
     * 托管总金额
     */
    private BigDecimal totalPrincipalAmount;

}
