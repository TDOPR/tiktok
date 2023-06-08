package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description 统计动静态收益信息
 * @CreateTime 2023/3/31 19:00
 **/
@Data
@Builder
@TableName("stat_dynamic_and_static")
@NoArgsConstructor
@AllArgsConstructor
public class StatDynamicAndStatic {

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
     * 总动态 t币
     */
    private BigDecimal dynamic;

    /**
     * 总静态 t币
     */
    private BigDecimal statics;

}
