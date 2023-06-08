package com.haoliang.pay.vn.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/18 11:42
 **/
@Data
public class VnCallBack {

    /**
     * 备注
     */
    private String message;

    /**
     * 随机字符
     */
    private String merchant_id;

    /**
     * 订单号
     */
    private String order_no;

    /**
     * 支付平台订单号
     */
    private String sys_order_no;

    /**
     * 金额
     */
    private String biz_amt;

    /**
     * 交易状态 processing;處理中     true: 成功       fail: 失敗
     */
    private String status;

    /**
     * 签名
     */
    private String sign;

}
