package com.haoliang.pay.th.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 泰国支付回调
 * @CreateTime 2023/4/20 10:35
 **/
@Data
public class ThPayCallBack {

    /**
     * 商户Id
     */
    private String mch_id;
    /**
     * 平台流水号
     */
    private String order_id;

    /**
     * 商户订单号
     */
    private String order_no;

    /**
     * 订单状态 1:待处理.2:成功.3:失败
     */
    private Integer state;
    /**
     * 实际交易金额(可能和提交金额不一致)
     */
    private Integer trade_amount;

    /**
     * 提示信息
     */
    private String message;

    /**
     * 签名
     */
    private String sign;

}
