package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/2/27 14:51
 **/
@Data
public class TiktokTaskPricesVO {
    /**
     * 套餐Id
     */
    private Integer id;

    /**
     * 数量
     */
    private Integer num;

    /**
     * 价格
     */
    private Integer price;
}
