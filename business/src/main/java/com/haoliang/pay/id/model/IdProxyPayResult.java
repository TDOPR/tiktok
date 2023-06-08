package com.haoliang.pay.id.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/14 16:05
 **/
@Data
public class IdProxyPayResult {

    /**
     * 请求业务是否成功
     */
    private String platRespCode;

    /**
     * 接口响应信息提示
     */
    private String platRespMessage;

    /**
     * 平台订单号
     */
    private String platOrderNum;

    /**
     * 商户订单号
     */
    private String orderNum;

    /**
     * 订单状态
     */
    private Integer status;

    /**
     *订单状态信息
     */
    private String statusMsg;

    /**
     * 代付金额
     */
    private String money;

    /**
     * 手续费
     */
    private String fee;

    /**
     * 手续费类型
     */
    private String feeType;

    /**
     * 银行编号
     */
    private String bankCode;

    /**
     * 客户银行卡号
     */
    private String number;

    /**
     * 客户名称
     */
    private String name;

    /**
     *按请求参数返回
     */
    private String 	description;

    /**
     *签名
     */
    private String platSign;

}
