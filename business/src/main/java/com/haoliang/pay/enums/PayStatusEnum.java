package com.haoliang.pay.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Dominick Li
 * @Description 法币支付状态
 * @CreateTime 2022/11/1 16:01
 **/
@Getter
@AllArgsConstructor
public enum PayStatusEnum {

    INIT_ORDER("INIT_ORDER",0,"订单初始化"),
    NO_PAY("NO_PAY", 1, "未支付"),
    SUCCESS("SUCCESS", 2, "支付成功"),
    PAY_CANCEL("PAY_CANCEL", 3, "撤销"),
    PAY_ERROR("PAY_ERROR", 4, "支付失败");

    private String value;

    private Integer status;

    private String name;

    public static PayStatusEnum nameOf(String value) {
        for (PayStatusEnum payStatusEnum : values()) {
            if (payStatusEnum.getValue().equals(value)) {
                return payStatusEnum;
            }
        }
        return null;
    }

    public static PayStatusEnum statusOf(Integer status) {
        for (PayStatusEnum payStatusEnum : values()) {
            if (payStatusEnum.getStatus().equals(status)) {
                return payStatusEnum;
            }
        }
        return null;
    }

}
