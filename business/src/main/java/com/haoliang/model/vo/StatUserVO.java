package com.haoliang.model.vo;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/12 18:56
 **/
@Data
public class StatUserVO {

    /**
     * 统计时间
     */
    private LocalDateTime now;

    /**
     * 总用户
     */
    private Integer total;

    /**
     * 有效用户
     */
    private Integer valid;

    /**
     * 零撸用户
     */
    private Integer zero;

    /**
     * 新增用户
     */
    private Integer yesterdayAdd;

    /**
     * 社区用户
     */
    private Integer community;
}
