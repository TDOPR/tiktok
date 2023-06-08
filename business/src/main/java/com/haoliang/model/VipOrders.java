package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author Dominick Li
 * @Description 用户购买的vip套餐记录
 * @CreateTime 2023/3/5 14:39
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("vip_orders")
public class VipOrders extends BaseModelCID {

    /**
     * 用户Id 收益
     */
    private Integer userId;

    /**
     * 购买的vip等级
     */
    private Integer level;

    /**
     * 收益上限总量
     */
    private BigDecimal total;

    /**
     * 剩余收益余量
     */
    private BigDecimal allowance;

    /**
     * 已接单未完成的冻结的收益
     */
    private BigDecimal frozenAmount;

    /**
     * 是否有效 1=有效 0=无效
     */
    private Integer valid;
}
