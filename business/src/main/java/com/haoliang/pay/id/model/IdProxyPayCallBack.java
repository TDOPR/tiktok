package com.haoliang.pay.id.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description 印尼支付后回调参数
 * @CreateTime 2023/4/13 15:21
 **/
@Data
public class IdProxyPayCallBack {

    /**
     * 到账银行
     */
    private String bankCode;

    /**
     * 订单描述
     */
    private String description;

    /**
     * 手续费
     */
    private String fee;
    /**
     * 订单结果状态 手续费类型, 0：订单内扣除 1：手续费另计
     */
    private String feeType;

    /**
     * 代付金额
     */
    private String money;

    /**
     * 插卡客户名称
     */
    private String name;

    /**
     * 客户银行卡号
     */
    private String number;

    /**
     * 商户订单号
     */
    private String orderNum;

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

    /**
     * 订单状态描述
     */
    private String statusMsg;
}
