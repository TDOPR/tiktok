package com.haoliang.pay.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Dominick Li
 * @Description 法币提现状态
 * @CreateTime 2022/11/1 16:01
 **/
@Getter
@AllArgsConstructor
public enum ProxyPayStatusEnum {

    INIT_ORDER(0, "待处理"),
    NO_PAY(1, "已受理"),
    SUCCESS(2, "代付成功"),
    PAY_CANCEL(4, "代付失败"),
    PAY_ERROR(5, "银行代付中"),

    TO_BE_REVIEWED(8, "待审核"),
    DELAY(9, "延迟T+2"),
    API_ERROR(10, "调用代付接口异常"),
    ;
    private Integer status;

    private String name;


}
