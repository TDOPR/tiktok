package com.haoliang.pay.th.enums;

import com.haoliang.pay.enums.PayStatusEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ThPayStatusEnum {

    PROCESSING(1, PayStatusEnum.NO_PAY.getStatus(), "待处理"),
    SUCCESS(2, PayStatusEnum.SUCCESS.getStatus(), "支付成功"),
    FAIL(3, PayStatusEnum.PAY_ERROR.getStatus(), "支付失败");


    /**
     * 支付平台状态
     */
    private Integer value;

    /**
     * 系统充值状态码
     */
    private Integer status;

    private String name;
    public static ThPayStatusEnum nameOf(Integer value) {
        for (ThPayStatusEnum thPayStatusEnum : values()) {
            if (thPayStatusEnum.getValue().equals(value)) {
                return thPayStatusEnum;
            }
        }
        return null;
    }


}
