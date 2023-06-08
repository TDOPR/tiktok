package com.haoliang.model.vo;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2022/12/20 15:30
 **/
@Data
public class PayInfoVO {

    /**
     * 支付详情信息二维码
     */
    private String url;

    /**
     * 订单id
     */
    private String orderId;

    public PayInfoVO(String url, String orderId) {
        this.url = url;
        this.orderId = orderId;
    }
}
