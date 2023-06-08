package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCIDNoModifyTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description 购买tiktok任务发布套餐记录表
 * @CreateTime 2023/2/27 14:39
 **/
@Data
@TableName("tiktok_task_price_orders")
@Builder
@AllArgsConstructor
public class TiktokTaskPriceOrders extends BaseModelCIDNoModifyTime {

    /**
     * 购买套餐Id
     */
    private Integer priceId;

    /**
     * 购买的用户Id
     */
    private Integer userId;

}
