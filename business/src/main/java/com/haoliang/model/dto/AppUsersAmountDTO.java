package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/11/25 15:15
 **/
@Data
public class AppUsersAmountDTO {

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 总业绩
     */
    private BigDecimal totalAmount;

    /**
     * 等级
     */
    private Integer vipLevel;

    /**
     * 社区等级
     */
    private Integer level;

}
