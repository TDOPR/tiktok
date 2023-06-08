package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description 收益信息
 * @CreateTime 2023/2/28 16:03
 **/
@Data
public class EarningsDTO {

    /**
     * 社区收益
     */
    private BigDecimal communityBenefits;

    /**
     * 任务收益
     */
    private BigDecimal taskBenefits;

}
