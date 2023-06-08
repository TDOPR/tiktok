package com.haoliang.pay.vn.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/18 12:54
 **/
@Data
public class VnPayStatusResult {

    /**
     * 请求结果 success=成功 false=失败
     */
    private String result;

    /**
     * 回传消息
     */
    private String message;

    /**
     * 商戶編號
     */
    private String merchant_id;

    /**
     * 客戶訂單編號
     */
    private String order_no;

    /**
     * 商城訂單編號
     */
    private String sys_order_no;

    /**
     * 訂單金額
     */
    private String biz_amt;

    /**
     * 状态  processing;處理中  true: 成功  fail:失败
     */
    private String status;

    /**
     * 簽名
     */
    private String sign;
}
