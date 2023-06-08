package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description 余额持有管理
 * @CreateTime 2023/3/31 19:31
 **/
@Data
@Builder
@TableName("stat_balance")
public class StatBalance {
    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * 创建日期
     */
    private LocalDate createDate;

    /**
     * usd金额
     */
    private BigDecimal usd;

    /**
     * ttt金额
     */
    private BigDecimal ttt;

    /**
     * ttt转usd金额
     */
    private BigDecimal tttToUsd;
}
