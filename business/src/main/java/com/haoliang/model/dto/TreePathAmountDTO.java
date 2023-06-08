package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/14 18:44
 **/
@Data
public class TreePathAmountDTO {

    /**
     * 托管金额
     */
    private BigDecimal totalAmount;

    /**
     * 用户Id
     */
    private Integer descendant;

    /**
     * 级别
     */
    private Integer level;
}
