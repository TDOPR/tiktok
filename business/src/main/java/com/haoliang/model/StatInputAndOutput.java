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
 * @Description 统计入金和出金
 * @CreateTime 2023/3/31 18:33
 **/
@Data
@Builder
@TableName("stat_input_and_output")
public class StatInputAndOutput {

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
     * 入金 Usd
     */
    private BigDecimal input;

    /**
     * 出金 Usd
     */
    private BigDecimal output;

    /**
     * 当日TVL 含TTT
     */
    private BigDecimal tvlTtt;

    /**
     * 当日TVL 不含TTT
     */
    private BigDecimal tvl;

    /**
     * Bubble 泡沫 含ttt减去不含ttt
     */
    private BigDecimal bubble;

}
