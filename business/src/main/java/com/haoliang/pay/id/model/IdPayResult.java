package com.haoliang.pay.id.model;

import lombok.Data;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/14 16:05
 **/
@Data
public class IdPayResult {

    /**
     * 请求业务是否成功
     */
    private String platRespCode;

    /**
     * 收银台链接
     */
    private String url;

    /**
     * 接口响应信息提示
     */
    private String platRespMessage;

    /**
     * 平台订单号
     */
    private String platOrderNum;

}
