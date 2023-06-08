package com.haoliang.pay.id.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 印尼支付后回调参数
 * @CreateTime 2023/4/13 15:21
 **/
@Data
public class IdPayCallBack {

    /**
     * 响应状态
     */
    private String code;
    /**
     * 邮箱号
     */
    private String email;
    /**
     * 支付方式
     */
    private String method;

    /**
     * 消息
     */
    private String msg;

    /**
     * 客户名称
     */
    private String name;

    /**
     * 商户订单号
     */
    private String orderNum;

    /**
     * 手续费
     */
    private String payFee;

    /**
     * 代收金额
     */
    private String payMoney;

    /**
     * 平台订单号
     */
    private String platOrderNum;

    /**
     * 平台签名
     */
    private String platSign;

    /**
     *状态
     */
    private String status;
}
