package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * @author Dominick Li
 * @Description 7天待领取冻结收益
 * @CreateTime 2022/11/1 10:57
 **/
@Data
@Builder
@TableName("freeze_proxy_logs")
@NoArgsConstructor
@AllArgsConstructor
public class FreezeProxyLogs {
    /**
     * 唯一标识
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * 创建时间
     */
    private LocalDate createTime;
    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 本次变动金额
     */
    private BigDecimal amount;

    /**
     * 流水类型 对应FlowingTypeEnum枚举里的动态奖励类型
     */
    private Integer type;

}
