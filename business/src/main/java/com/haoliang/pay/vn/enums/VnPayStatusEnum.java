package com.haoliang.pay.vn.enums;

import com.haoliang.pay.enums.PayStatusEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/18 16:11
 **/
@Getter
@AllArgsConstructor
public enum VnPayStatusEnum {

    //processing;處理中     true: 成功       fail: 失敗

    PROCESSING("processing", PayStatusEnum.NO_PAY.getStatus(), "未支付"),
    SUCCESS("true", PayStatusEnum.SUCCESS.getStatus(), "支付成功"),
    FAIL("fail", PayStatusEnum.PAY_ERROR.getStatus(), "支付失败")
    ;

    private String value;

    private Integer status;

    private String name;

    public static VnPayStatusEnum nameOf(String value) {
        for (VnPayStatusEnum vnPayStatusEnum : values()) {
            if (vnPayStatusEnum.getValue().equals(value)) {
                return vnPayStatusEnum;
            }
        }
        return null;
    }

}
