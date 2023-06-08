package com.haoliang.model.dto;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/16 11:35
 **/
@Data
public class AppUserTaskDTO {
    /**
     * 任务Id
     */
    private Long id;

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * ttt奖励
     */
    private BigDecimal amount;

    /**
     * 对应使用的vip套餐Id列表
     */
    private String vipOrderIds;

    /**
     * 任务类型
     */
    private Integer type;

    /**
     * 是否为零撸用户
     */
    private Integer valid;

}
