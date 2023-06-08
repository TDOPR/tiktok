package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description 统计用户数量信息
 * @CreateTime 2023/3/31 15:18
 **/
@Data
@Builder
@TableName("stat_user")
public class StatUser {

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
     * 昨日新增
     */
    private Integer yesterdayAdd;

    /**
     * 社区用户
     */
    private Integer community;

}
