package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/5 18:16
 **/
@Data
public class UserWalletsDTO {

    /**
     * 所属用户Id
     */
    private Integer userId;

    /**
     * 钱包余额
     */
    private BigDecimal walletAmount;

    /**
     * 代理商等级
     */
    private Integer level;

}
