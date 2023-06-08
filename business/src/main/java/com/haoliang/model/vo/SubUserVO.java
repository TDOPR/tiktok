package com.haoliang.model.vo;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/10 12:08
 **/
@Data
public class SubUserVO {

    /**
     * 下级团队业绩
     */
    private BigDecimal subTeamAmount;

    /**
     * 下级团队数量
     */
    private Integer subTeamSum;
}
