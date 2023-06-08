package com.haoliang.model.vo;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/15 11:30
 **/
@Data
@AllArgsConstructor
public class PayResultVO {

    /**
     * 订单号
     */
    private String orderNo;

    /**
     * 支付链接
     */
    private String payUrl;

}
