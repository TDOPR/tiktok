package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description 动态收益发放任务表
 * @CreateTime 2022/12/5 16:04
 **/
@Data
@TableName("business_job")
public class BusinessJob {

    /**
     * 创建时间
     */
    @TableId(type = IdType.NONE)
    private LocalDate createDate;

    /**
     * 发放代数奖任务是否执行 1=已执行 0=未执行
     */
    private Integer algebraTask;

    /**
     * 发放团队奖任务是否执行 1=已执行 0=未执行
     */
    private Integer teamTask;

    /**
     * 发放分红将任务是否执行 1=已执行 0=未执行
     */
    private Integer specialTask;

    /**
     * 持币奖
     */
    private Integer holdingCoinTask;
}
