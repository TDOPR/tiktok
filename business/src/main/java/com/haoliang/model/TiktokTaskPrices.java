package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModelCID;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description tiktok任务套餐价格
 * @CreateTime 2023/2/27 14:39
 **/
@Data
@TableName("tiktok_task_prices")
public class TiktokTaskPrices extends BaseModelCID {

    /**
     * 数量
     */
    private Integer num;

    /**
     * 金额
     */
    private Integer price;

    /**
     * 是否隐藏 1=显示 0=隐藏
     */
    private Integer visible;

}
